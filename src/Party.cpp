//
// nazghul - an old-school RPG engine
// Copyright (C) 2002, 2003 Gordon McNutt
//
// This program is free software; you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 2 of the License, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
// more details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Foundation, Inc., 59 Temple Place,
// Suite 330, Boston, MA 02111-1307 USA
//
// Gordon McNutt
// gmcnutt@users.sourceforge.net
//
#include "Party.h"
#include "dice.h"
#include "place.h"
#include "player.h"
#include "combat.h"
#include "species.h"
#include "occ.h"
#include "wind.h"
#include "vehicle.h"
#include "console.h"
#include "formation.h"
#include "ctrl.h"
#include "session.h"
#include "sched.h"
#include "log.h"
#include "event.h"
#include "cmd.h"
#include "cmdwin.h"
#include "factions.h"

#include <stdio.h>

#define DIRLOC(dir,place,coord) { \
        if ((dir) < 0) \
                (coord) = place_w((place)) - 1; \
        else if (! (dir)) \
                (coord) = place_w((place)) / 2; \
        else \
                (coord) = 0; \
}

// --------------------------------------------------------------------------
//
// PartyType
//
// --------------------------------------------------------------------------

PartyType::PartyType()
{
        setup();
}
PartyType::PartyType(char *tag, char *name, struct sprite *sprite)
        : ObjectType(tag, name, sprite, being_layer)
{
        setup();
}

void PartyType::setup()
{
        list_init(&groups);
        sleep_sprite = NULL;
        pmask = 0xff;
        vrad = 0;
        visible = false;
        speed = 0x7fffffff;
}

PartyType::~PartyType()
{
        i_group = groups.next;
        while (i_group != &groups) {
                struct GroupInfo *info = outcast(i_group, struct GroupInfo, list);
                i_group = i_group->next;
                if (info->dice)
                        free(info->dice);
                if (info->ai)
                        closure_unref(info->ai);
                delete info;
        }
}

#if 0
bool PartyType::init(class Character * ch)
{
	char name[64];

	// Use the species and occupation as the name of the party.
	snprintf(name, sizeof(name), "%s %s", ch->species->name, 
                 ch->occ ? ch->occ->name : "");

	if (!ObjectType::init("fake_tag", name, being_layer, ch->getSprite()))
		return false;

	pmask = ch->getPmask();
	speed = ch->getSpeed();
	vrad = ch->getVisionRadius();
	visible = ch->isVisible();
        sleep_sprite = ch->species->sleep_sprite;
	return true;
}
#endif

struct GroupInfo *PartyType::enumerateGroups(void)
{
	i_group = &groups;
	return getNextGroup();
}

struct GroupInfo *PartyType::getNextGroup(void)
{
	i_group = i_group->next;
	if (i_group != &groups)
                return outcast(i_group, struct GroupInfo, list);
	return NULL;
}

class Object *PartyType::createInstance()
{
	class Party *obj = new Party();
	if (!obj)
                return NULL;

        obj->init(this);

	return obj;
}


bool PartyType::isType(int classID)
{
        return (classID == PARTY_TYPE_ID);
}

int PartyType::getType()
{
        return PARTY_TYPE_ID;
}

int PartyType::getVisionRadius()
{
        return vrad;
}

bool PartyType::isVisible()
{
        return visible;
}

void PartyType::addGroup(struct species *species, struct occ *occ, struct sprite *sprite, char *dice, struct closure *ai)
{
        struct GroupInfo *group = new struct GroupInfo;
        assert(group);
        list_init(&group->list);
        group->species = species;
        group->occ = occ;
        group->sprite = sprite;
        group->dice = strdup(dice);
        group->ai = ai;
        if (ai)
                closure_ref(ai);
        list_add(&groups, &group->list);
        vrad = max(vrad, species->vr);
        visible = species->visible || visible;
        speed = min(speed, species->spd);
}

// --------------------------------------------------------------------------
//
// Party
//
// --------------------------------------------------------------------------


Party::Party()
{
        setup();
}

Party::Party(class PartyType *type, int faction, class Vehicle *_vehicle)
        : Being(type)
{
        setup();
        
        setBaseFaction(faction);

        // Vehicle
        if (!_vehicle) {
                vehicle = NULL;
        } else {
                vehicle = _vehicle;
                vehicle->occupant = this;
        }

        // Create the party members.
        createMembers();
}

void Party::setup()
{
	act = WORKING;
	fdx = 0;
	fdy = 0;
	size = 0;
	isWrapper = false;
	node_init(&members);
        n_members = 0;
	formation = NULL;
        wandering = false;
        memset(&pinfo, 0, sizeof(pinfo));
        ctrl = ctrl_party_ai;
        vehicle = NULL;
}

static bool party_remove_member(class Character *ch, void *data)
{
        ((Party*)data)->removeMember(ch);
        if (! ch->refcount)
                delete ch;
        return false;
}

Party::~Party()
{
        /* Dereference all the members by removing them from the party. If no
         * other container references them then they will be automatically
         * destoyed. */
        forEachMember(party_remove_member, this);
}

bool Party::isType(int classID)
{
        if (classID == PARTY_ID)
                return true;
        return Object::isType(classID);
}

int Party::getType()
{
        return PARTY_ID;
}

class PartyType *Party::getObjectType()
{
        return (class PartyType *) Object::getObjectType();
}

int Party::getVisionRadius()
{
        return getObjectType()->getVisionRadius();
}

void Party::init(int x, int y, struct place *place, class PartyType * type)
{
        Object::init(x, y, place, type);
}

void Party::setFleeVector(int x, int y)
{
        fdx = x;
        fdy = y;
}

void Party::getFleeVector(int *x, int *y)
{
        *x = fdx;
        *y = fdy;
}

int Party::getSize(void)
{
        return size;
}


bool Party::turn_vehicle(void)
{
	int cost = 0;

	// Three possible outcomes:
	// 
	// 1. We do NOT turn any vehicle, therefore we want to continue
	// processing the move() command which calls us and we do NOT want to
	// consume any turns (e.g., no vehicle, or horse turning north/south).
	// 
	// 2. We DO turn a vehicle, but it does not cost us anything (e.g.,
	// horse turning east/west).
	// 
	// 3. We do turn a vehicle and it cost us a turn (e.g., ship).

	if (!vehicle || !vehicle->turn(dx, dy, &cost) || !vehicle->mustTurn())
		return false;

        action_points -= cost;

	return true;
}

MoveResult Party::move(int dx, int dy)
{
	struct place *newplace;
	int newx;
	int newy;
	struct place *oldplace;
	int oldx;
	int oldy;
	class Moongate *moongate;
	class Object *mech;

	this->dx = dx;
	this->dy = dy;

	/* Check if the party is in a vehicle that must turn its facing before
	 * moving */
	if (turn_vehicle())
		return ChangedFacing;

	/* Remember old (current) coordinates */
	oldplace = getPlace();
	oldx = getX();
	oldy = getY();

	/* Setup new coordinates */
	newx = place_wrap_x(oldplace, oldx + dx);
	newy = place_wrap_y(oldplace, oldy + dy);
	newplace = oldplace;

	/* Walking off the edge of a map */
	if (place_off_map(oldplace, newx, newy)) {
		return OffMap;
	}

	/* Check if the player is there. */
	if (newx == player_party->getX() && 
            newy == player_party->getY()) {

                // ------------------------------------------------------------
                // Subtle: check if the player party is on the map. This
                // catches the case where the player has just engaged another
                // npc party in combat on this turn. I don't want this npc
                // party to move to that spot because then when the player
                // party exits combat they will be on top of this npc party.
                // ------------------------------------------------------------
                
                if (! player_party->isOnMap())
                        return WasOccupied;

		/* If this party is hostile to the player then begin combat */
		if (are_hostile(this, player_party)) {

			struct move_info info;
			struct combat_info cinfo;

			memset(&info, 0, sizeof(info));
			info.place = getPlace();
			info.x = newx;
			info.y = newy;
			info.dx = dx;
			info.dy = dy;
                        info.px = newx;
                        info.py = newy;
			info.npc_party = this;

			memset(&cinfo, 0, sizeof(cinfo));
			cinfo.defend = true;
			cinfo.move = &info;

			combat_enter(&cinfo);
                        endTurn();
                        //destroy();
                        return EngagedEnemy;
		}

                if (player_party->isResting()) {

                        /* If the player is sleeping then kick him out of
                         * bed */
                        player_party->throw_out_of_bed();

                }

                /* Else abort the move */
                return WasOccupied;

	}

	/* Check if another entity is already there */
	if (place_is_occupied(oldplace, newx, newy)) {
		return WasOccupied;
	}

	/* Check for a vehicle. */
	class Vehicle *veh = place_get_vehicle(newplace, newx, newy);
	if (veh && (vehicle || veh->occupant)) {
		return WasOccupied;
	}

	/* Check passability */
	if (!place_is_passable(oldplace, newx, newy, this,
                               PFLAG_MOVEATTEMPT |
			       (act == COMMUTING ? PFLAG_IGNOREMECHS : 0))) {
		return WasImpassable;
	}

        /* When wandering, don't wander over terrain hazards. When pathfinding,
         * assume that braving the hazard is the best course. */
	if (wandering && place_is_hazardous(newplace, newx, newy))
                return AvoidedHazard;


	// Check for a mech (not for passability, for sending the STEP
        // signal)
	mech = place_get_object(getPlace(), newx, newy, mech_layer);
        if (mech)
                mech->step(this);

	relocate(newplace, newx, newy);

	action_points -= place_get_movement_cost(getPlace(), getX(), getY(), this);

        return MovedOk;
}

bool Party::gotoSpot(int mx, int my)
{
	// Common routine used by work() and commute().
	struct astar_node *path;
	struct astar_node *next;
	struct astar_search_info as_info;
	int dx;
	int dy;
        enum MoveResult ret = NotApplicable;

	/* Look for a path. */
	memset(&as_info, 0, sizeof(as_info));
	as_info.x0 = getX();
	as_info.y0 = getY();
	as_info.x1 = mx;
	as_info.y1 = my;
	as_info.flags = PFLAG_IGNOREMECHS;
	path = place_find_path(Place, &as_info, this);

	if (!path)
		return false;

        //dump_path(path);

	/* The first node in the path is the starting location. Get the next
	 * step. */
	next = path->next;
	if (next) {

		/* Get the movement vector */
		dx = next->x - getX();
		dy = next->y - getY();

		/* Attempt to move */
		ret = move(dx, dy);
                        
	}

	/* Cleanup */
	astar_path_destroy(path);

	return (ret == MovedOk || ret == EngagedEnemy);
}

bool Party::attack_with_ordnance(int d)
{
	class ArmsType *ordnance;
	int dx, dy;
	bool ret;

	if (!vehicle || !(ordnance = vehicle->getOrdnance()))
		return false;

	// Check if the player is in range.
	if (d > ordnance->getRange()) {
		return false;
	}
	// Get the normalized vector to the player.
        place_get_direction_vector(getPlace(),
                                   getX(), getY(), 
                                   player_party->getX(), player_party->getY(),
                                   &dx, &dy);
	clamp(dx, -1, 1);
	clamp(dy, -1, 1);

	// Check if the player is on a major axes (assumes we must fire in a
	// straight line -- always true for now).
	if (player_party->getY() == getY()) {

		// If necessary, turn the vehicle to broadside the player (this
		// assumes we must use a broadside, again always true for now).
		// If we do turn return true to end this turn.
		if (vehicle->getFacing() != NORTH &&
		    vehicle->getFacing() != SOUTH) {
			int cost;
			vehicle->turn(0, 1, &cost);
			action_points -= cost;
			return true;
		}

		ret = vehicle->fire_weapon(dx, dy, this);
		assert(ret);	// to remind me if I change some assumptions
		return true;
	}
	// Ditto but for the other axis.
	if (player_party->getX() == getX()) {
		if (vehicle->getFacing() != EAST &&
		    vehicle->getFacing() != WEST) {
			int cost;
			vehicle->turn(1, 0, &cost);
			return true;
		}
		ret = vehicle->fire_weapon(dx, dy, this);
		assert(ret);	// to remind me if I change some assumptions
		return true;
	}
	// In range but no lined up on an axis. For now just return and let out
	// strategy be to close with the player's ship. In future I might want
	// to try and go to find and move toward the nearest axis point.
	return false;
}


void Party::exec()
{        
        assert(!isDestroyed());

        startTurn();

        while (action_points > 0 && !isDestroyed()) {

                int initial_points = action_points;
                ctrl(this);
                /* If we didn't use any action points then we're idle and need
                 * to break to prevent an endless loop. */
                if (action_points == initial_points)
                        break;
	}

        /* Objects cannot save action points (but they can be in debt). */
        if (action_points > 0)
                action_points = 0;
}

void Party::init(class PartyType * type)
{
	Object::init(type);
}

bool Party::createMembers(void)
{
	int order = 0;
	class PartyType *type = getObjectType();
	char name[64];
	static int instance = 0;

	for (struct GroupInfo * ginfo = type->enumerateGroups(); ginfo;
	     ginfo = type->getNextGroup()) {

		int n;

		// For each group in the party, randly generate the number of
		// NPCs from that group based on its max size (need at least
		// one). Then create that many NPC characters and add them to
		// the party.
                n = dice_roll(ginfo->dice);

		while (n) {

			// Create and initialize a new "stock" character.
			class Character *c = new class Character();
			if (!c)
				break;
			snprintf(name, sizeof(name), "%s %s %d",
				 ginfo->species->name,
				 ginfo->occ ? ginfo->occ->name : "",
				 instance++);
			if (!(c->initStock(ginfo->species, ginfo->occ,
					   ginfo->sprite, name, order)))
				return false;

                        if (ginfo->ai) {
                                c->setAI(ginfo->ai);
                        }

                        addMember(c);
			order++;
			n--;
		}
	}

	return true;
}


void Party::forEachMember(bool(*fx) (class Character *, void *), void *data)
{
	struct node *elem;

	elem = members.next;
	while (elem != &members) {
		class Character *c;

		c = (class Character *)elem->ptr;
                elem = elem->next;

		if (fx(c, data))
			return;
	}
}

static bool party_destroy_and_remove_member(class Character * c, void *data)
{
	class Party *party = (class Party *) data;

	c->destroy();
	party->removeMember(c);

        // Note: between destroying and removing it the ref count probably
	// dropped to zero and already deleted the object. If not, something
	// else might have a legitimate reference to it, so don't delete.

        // delete c;

	return false;
}

void Party::destroy()
{
        // Note: this is a case of destroying an object in a container:
	disembark();

	forEachMember(party_destroy_and_remove_member, this);
	assert(node_list_empty(&members));
	Object::destroy();	// removes it
}

void Party::removeMember(class Character * c)
{
        struct node *node;

        /* Convenienve pointer to node */
        node = c->plnode;
        
        /* Should be valid */
        assert(node);

        /* Unlink the node from the member list */
	node_remove(node);

        /* Break the link from the char back to the node */
        c->plnode = NULL;

        /* Break the link from the char back to the party */
	c->party = NULL;

        /* Reduce party size counter */
	size--;

        /* Release the node */
        node_unref(node);

        /* Release the char */
        obj_dec_ref(c);
}

bool Party::addMember(class Character * c)
{
        struct node *node;

        /* Add ref to char to prevent destruction */
        obj_inc_ref(c);

        /* Make a new list node for the member list */
        node = node_new(c);

        /* Link the new member in at the END of the list (otherwise, in the
         * case of the player party, the order shown in status gets screwed
         * up) */
        node_add_tail(&members, node);

        /* Point the member back to its node (for fast removal) */
        c->plnode = node;

        /* Point the member back to its party */
        c->party = this;

        /* Set the character's order in the party */
        c->setOrder(size);

        /* Increase the party size counter */
        size++;

        /* Make the member loyal to the party */
        c->setBaseFaction(getBaseFaction());

        return true;
}

static bool add_to_player_party(class Character * c, void *data)
{
	// Note: I'll leave the party set as-is. It does no harm and might do
	// some good later.  Note: The order will be forgotten (changed to
	// match player party order).

	if (!c->joinPlayer())
		assert(false);
	return false;
}

bool Party::joinPlayer(void)
{
	remove();
	forEachMember(add_to_player_party, 0);
	return true;
}

void Party::paint(int sx, int sy)
{
	if (vehicle)
		vehicle->paint(sx, sy);
	else
		Object::paint(sx, sy);
}

struct sprite *Party::getSprite()
{
	if (vehicle)
		return vehicle->getSprite();

        if (act == SLEEPING && getObjectType()->sleep_sprite)
                return getObjectType()->sleep_sprite;

	return type->getSprite();
}

void Party::disembark()
{
	if (vehicle) {
		assert(getPlace());
		vehicle->occupant = 0;
                if (vehicle->isDestroyed()) {
                        // This happens when the vehicle has been destroyed,
                        // and it called destroy() on us, and we then called
                        // disambark().
                        delete vehicle;
                } else {
                        vehicle->relocate(getPlace(), getX(), getY());
                }
		vehicle = 0;
	}
}

int Party::getSpeed()
{
	if (vehicle)
		return vehicle->getSpeed();
	return getObjectType()->getSpeed();
}

struct damage_member_info {
	int damage;
	bool any_alive;
};

static bool damage_member(class Character * member, void *data)
{
	struct damage_member_info *dm_info = (struct damage_member_info *) data;

	// apply damage
	member->damage(dm_info->damage);

	// check if dead and remove from party
	if (member->isDead()) {
		member->party->removeMember(member);
		delete member;
		return false;
	}
	// otherwise at least one still alive
	dm_info->any_alive = true;

	return false;
}

void Party::damage(int damage)
{
	struct damage_member_info dm_info;

	// First apply damage to the vehicle. If the vehicle is destroyed then
	// destroy the party, too.
	if (vehicle) {
		vehicle->damage(damage);
                
                // If the vehicle was destroyed by the above damage, it has
                // already called destroy() on its occupants (that's us right
                // here!) and we've already disembarked. So there's really
                // nothing to do.
                return;

	}

	// Apply damage to all party members. If they all die then the party is
	// destroyed, too.
	dm_info.damage = damage;
	dm_info.any_alive = false;
	forEachMember(damage_member, &dm_info);
	if (!dm_info.any_alive) {
		destroy();
	}
}

void Party::distributeMembers()
{
        // -------------------------------------------------------------------
        // Emulate what I currently do for the player party.
        // -------------------------------------------------------------------

        // -------------------------------------------------------------------
        // The combat alg requires me to fill out a "position info" structure
        // based on the player party destination.
        // -------------------------------------------------------------------
        
        combat_fill_position_info(&pinfo, getPlace(), getX(), getY(), dx, dy, false);

        // -------------------------------------------------------------------
        // Set the party formation to a sane default.
        // -------------------------------------------------------------------

        if (NULL == pinfo.formation)
                pinfo.formation = formation_get_default();

        // -------------------------------------------------------------------
        // Party members must be placed such that they can pathfind back to the
        // party. This minimizes the chance of a party member getting stranded
        // (which in turn will strand the whole party in that place).
        // -------------------------------------------------------------------

        pinfo.find_party = true;

        // -------------------------------------------------------------------
        // Remove the party from the current place before distributing members.
        // -------------------------------------------------------------------

        remove();
        mapSetDirty();

        // -------------------------------------------------------------------
        // Use the combat algorithm to place each member. Currently this will
        // never fail, in the degenerate case all party members will end up
        // "stranded" on top of the destination tile.
        // -------------------------------------------------------------------

        forEachMember(combat_place_character, &pinfo);

}

struct formation *Party::get_formation()
{
	if (vehicle && vehicle->get_formation())
		return vehicle->get_formation();
	return getObjectType()->formation;
}

void Party::describe()
{
        Object::describe();
        if (vehicle) {
                log_continue(" in ");
                vehicle->describe();
        }
}

static bool get_member_movement_sound(class Character * member, void *data)
{
        sound_t **sound = (sound_t **)data;
	*sound = member->get_movement_sound();
        return data != 0;
}


sound_t *Party::get_movement_sound()
{
        sound_t *sound = NULL_SOUND;

	if (vehicle)
		return vehicle->get_movement_sound();
        forEachMember(get_member_movement_sound, &sound);
        return sound;
}

int Party::getActivity()
{
        return act;
}

static bool member_burn(class Character *member, void *data)
{
        member->burn();
        return false;
}

static bool member_sleep(class Character *member, void *data)
{
        member->sleep();
        return false;
}

static bool member_apply_existing(class Character * pm, void *data)
{
        if (pm->isAsleep()) {
                if ((rand() % 100) < PROB_AWAKEN) {
                        pm->awaken();
                }
        }
	return false;
}

void Party::burn()
{
        forEachMember(member_burn, NULL);
        if (allDead())
                destroy();
}

void Party::sleep()
{
        forEachMember(member_sleep, NULL);
        if (allDead())
                destroy();
}

static bool member_check_if_alive(class Character *member, void *data)
{
        if (!member->isDead()) {
                *((bool*)data) = false;
                return true;
        }
        return false;
}

bool Party::allDead()
{
        bool dead = true;
        forEachMember(member_check_if_alive, &dead);
        return dead;
}

void Party::switchOrder(class Character *ch1, class Character *ch2)
{
        int tmp;
        node_switch(ch1->plnode, ch2->plnode);
        tmp = ch1->getOrder();
        ch1->setOrder(ch2->getOrder());
        ch2->setOrder(tmp);
}

char *Party::getName()
{
        return Object::getName();
}

/* Convenience macro for iterating over party members: */
#define FOR_EACH_MEMBER(e,c)                                               \
   for ((e) = members.next, (c) = (class Character *)(e)->ptr;             \
        (e) != &members;                                                   \
        (e) = (e)->next, (c) = (class Character *)(e)->ptr)

// --------------------------------------------------------------------
// Wherever the party is, there shall the members be also.
// --------------------------------------------------------------------

void Party::setPlace(struct place *place)
{
        struct node *entry;
        class Character *member;
        Object::setPlace(place);
        FOR_EACH_MEMBER(entry, member) {
                member->setPlace(place);
        }
}

void Party::setX(int x)
{
        struct node *entry;
        class Character *member;
        Object::setX(x);
        FOR_EACH_MEMBER(entry, member) {
                member->setX(x);
        }
}

void Party::setY(int y)
{
        struct node *entry;
        class Character *member;
        Object::setY(y);
        FOR_EACH_MEMBER(entry, member) {
                member->setY(y);
        }
}

bool Party::addEffect(struct effect *effect, struct gob *gob)
{
        struct node *entry;
        class Character *member;
        bool result = false;

        // NOTE: in the future we'll probably want to distinguish between
        // start-of-char-turn and start-of-party-turn for characters. Also,
        // we'll want to specify if the hook should really apply to the party
        // object or to its members.
        FOR_EACH_MEMBER(entry, member)
                result = member->addEffect(effect, gob) || result;
        
        return result;
}

bool Party::removeEffect(struct effect *effect)
{
        struct node *entry;
        class Character *member;
        bool result = false;

        FOR_EACH_MEMBER(entry, member) {
                result = member->removeEffect(effect) || result;
        }
        
        return result;
}

void Party::startTurn()
{
        struct node *entry;
        class Character *member;

        Object::startTurn();
        if (isDestroyed())
                return;

        // NOTE: in the future we'll probably want to distinguish between
        // start-of-char-turn and start-of-party-turn for characters. Also, to
        // be authentic we really should iterate over this in proportion to the
        // map scale.
        FOR_EACH_MEMBER(entry, member)
                member->runHook(OBJ_HOOK_START_OF_TURN);

        if (allDead())
                destroy();

}

void Party::applyEffect(closure_t *effect)
{
        struct node *entry;
        class Character *member;

        FOR_EACH_MEMBER(entry, member)
                member->applyEffect(effect);
}

void Party::save(struct save *save)
{
        save->enter(save, "(kern-mk-party %s %d\n",
                    getObjectType()->getTag(), getBaseFaction());
        if (vehicle)
                vehicle->save(save);
        else
                save->write(save, "nil\n");
        save->exit(save, ")\n");
}

static bool member_remove(class Character *member, void *data)
{
        member->remove();
        return false;
}

void Party::removeMembers()
{
        forEachMember(member_remove, NULL);
}

static bool memberStart(class Character *member, void *data)
{
        member->start();
        return false;
}

void Party::start()
{
        forEachMember(memberStart, NULL);
}

int Party::getMovementCost(int pclass)
{
        struct node *entry;
        class Character *member;
        int maxCost = 0;

        if (vehicle)
                return vehicle->getMovementCost(pclass);

        FOR_EACH_MEMBER(entry, member) {
                if (!member->isDead()) {
                        int cost = member->getMovementCost(pclass);
                        maxCost = max(cost, maxCost);
                }
        }

        return maxCost;
}

class Character *Party::getMemberByOrder(int order)
{
        struct node *entry;
        class Character *member;

        FOR_EACH_MEMBER(entry, member) {
                if (! order)
                        return member;
                order--;
        }

        return NULL;
}

Object *Party::getSpeaker()
{
        struct node *entry;
        class Character *member;
        struct stat_list_entry *statlist;
        int list_sz = 0;
        class Character *selected = NULL;
        enum StatusMode orig_stat_mode;
	struct KeyHandler kh;
	struct ScrollerContext sc;

        // Allocate an array of status list entries big enough for the entire
        // party (this is probably more than we need, but it's only temporary).
        statlist = (struct stat_list_entry*)
                calloc(getSize(), sizeof(struct stat_list_entry));
        assert(statlist);


        // For each party member that has a conversation, add it to the list.
        FOR_EACH_MEMBER(entry, member) {
                conv = member->getConversation();
                if (! conv)
                        continue;

                statlist[list_sz].sprite = member->getSprite();
                snprintf(statlist[list_sz].line1, STAT_MAX_CHARS_PER_LINE,
                         member->getName());
                statlist[list_sz].data = member;
                list_sz++;
        }

        // Check if nobody has a conversation.
        if (! list_sz)
                goto done;

        // Check if only one has a conversation.
        if (list_sz == 1) {
                selected = (class Character*)statlist[0].data;
                goto done;
        }

        // The player has to choose. Poke the list into the status state.
        statusSetGenericList(list_sz, statlist);

        // Remember the current stat mode so we can restore it.
        orig_stat_mode = statusGetMode();

        // Switch the status mode over to list selection.
        statusSetMode(GenericList);

        // Setup a keyhandler for handling the scrolling
	sc.selector  = Generic;
	sc.selection = NULL;
	kh.fx        = scroller;
	kh.data      = &sc;

        // Push the handler and wait for the player to make a selection.
	eventPushKeyHandler(&kh);
	cmdwin_print("<select>");
	eventHandle();
	cmdwin_backspace(strlen("<select>"));
	eventPopKeyHandler();

	statusRepaint();

	selected = (class Character *) sc.selection;
        
 done:
        statusSetMode(orig_stat_mode);
        free(statlist);
        return selected;
}
