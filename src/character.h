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
#ifndef character_h
#define character_h

#include "Being.h"
#include "Arms.h"
#include "species.h"
#include "clock.h"

#undef RAM_ATTACK

// Weird C++-isms ensue at compile-time if I try to include combat.h directly
// here...
extern void combatKIA(class Character *);

struct ready_spell {
	struct list list;
	struct spell *spell;
};

struct point {
	int x, y;
};

struct TypicalObjectInfo {
	int probability;
	class ObjectType *type;
	int n_max;
};


class Character:public Being {
 public:

	enum ReadyResult {
		Readied,
		NoAvailableSlot,
		WrongType,
		TooHeavy,
	};

	Character();
        Character(char *tag, char *name, 
                  struct sprite *sprite,
                  struct species *species, struct occ *occ,
                  int str, int intl,
                  int dex, int hpmod, int hpmult, 
                  int mpmod, int mpmult, 
                  int hp, int xp, int mp,
                  int lvl);
	virtual ~ Character();

        virtual int getActivity();
        virtual int getArmor();
        virtual int getDefend();
        virtual class Party * getParty();
	virtual int getType();
	virtual char *getName();
	virtual int getHp();
	virtual int getMaxHp();
	virtual int getOrder();
	virtual void addExperience(int delta);
	virtual int getExperience();
	virtual unsigned char getStrength();
	virtual unsigned char getIntelligence();
	virtual unsigned char getDexterity();
	virtual unsigned char getLevel();
        virtual struct mmode *getMovementMode();
	virtual int getArmourClass();
	virtual char *getWoundDescription();
	virtual class Character *getAttackTarget();
	virtual class ArmsType *getNextArms();
	virtual class ArmsType *getNextWeapon();
	virtual class ArmsType *getCurrentWeapon();
	virtual int getLight();
	virtual int getVisionRadius();
	virtual int getSpeed();
	virtual int getMana();
	virtual int getMaxMana();
	virtual struct sprite *getSprite();
	virtual int getFleeDx();
	virtual int getFleeDy();
	virtual int getRestCredits(void);
        virtual char *get_damage_sound();
        virtual char *get_movement_sound();


        virtual bool isCharmed();
        virtual bool isCamping();
        virtual bool isCompanionOf(class Object *other);
        virtual bool isGuarding();
        virtual bool isLeader();
        virtual bool isResting();
	virtual bool isType(int classID);
	virtual bool isDead();
	virtual bool isAsleep();
	virtual bool isIncapacitated();
	virtual bool isPlayerControlled();
	virtual bool hasReadied(class ArmsType * arms);
	virtual bool isSolo();
	virtual int  hasAmmo(class ArmsType * weapon);
        virtual bool hasInInventory(class ObjectType *type);
	virtual bool isFleeing();
	virtual bool wasElevated(void);
	virtual bool isVisible();
	virtual bool isShaded();
        virtual bool isPlayerPartyMember();
        virtual bool canWanderTo(int newx, int newy);

        virtual void addDefense(int val);
	virtual void changeArmourClass(int val);
	virtual void changeSleep(bool val);
	virtual void setLight(int delta);
	virtual void addMana(int delta);
	virtual void addRestCredits(int delta_hours);

        virtual void setActivity(int val);
        virtual void setControlMode(enum control_mode mode);
        virtual void setDefaultCondition();
        virtual void setLeader(bool val);
	virtual void setName(char *name);
        virtual void setSchedule(struct sched *sched);
        virtual void setSolo(bool val);
	virtual void setOrder(int order);
	virtual void setHp(int hp);
	virtual void setPlayerControlled(bool val);
	virtual void setAttackTarget(class Character * target);
	virtual void setFleeing(bool val);
	virtual void setCombat(bool val);
	virtual void setRestCredits(int hours);
	virtual void setElevated(bool val);
	//virtual void setVisible(bool val);
        virtual void ambushWhileCamping();
	virtual void awaken(void);
        virtual void beginCamping(int hours);
        virtual void beginGuarding(int hours);
        virtual void beginResting(int hours);        
	virtual void damage(int amount);
        virtual void endCamping(void);
        virtual void endGuarding();
        virtual void endResting(void);
        virtual void exec(struct exec_context *context);
        virtual void groupExitTo(struct place *dest_place, 
                                 int dest_x, int dest_y,
                                 struct closure *closure);
        virtual void heal(int amount);
        virtual bool joinPlayer(void);
	virtual void kill();
	virtual void remove();
	virtual enum ReadyResult ready(class ArmsType * arms);
	virtual void synchronize();
	virtual bool unready(class ArmsType * arms);
	virtual enum MoveResult move(int dx, int dy);
	virtual enum MoveResult flee();
	virtual void attackTerrain(int x, int y);
	virtual void initItems();
	virtual void armThyself();
	virtual bool needToRearm();
	virtual void resurrect();
	virtual void rest(int hours);
	virtual class Object *clone();
	virtual void describe();
        virtual void burn();
        virtual void save(struct save *save);
        virtual void sleep();
        virtual bool tryToRelocateToNewPlace(struct place *place, 
                                             int x, int y,
                                             struct closure *closure);
	virtual class ArmsType *enumerateArms();
	virtual class ArmsType *enumerateWeapons();
        virtual bool addFood(int quantity);
        virtual bool add(ObjectType *type, int amount);
        virtual bool takeOut(ObjectType *type, int amount);
        virtual int getCurrentFaction();

        void charm(int faction);
        void unCharm();
	void useAmmo(class ArmsType *weapon);
        bool canSee(class Object *obj);
	bool gotoSpot(int x, int y);
	bool commute();
	bool initStock(struct species * species, struct occ * occ,
		       struct sprite * sprite, char *name, int order);

	char *tag;
	struct list plist;	// party list
	struct list llist;	// load list
	class Party *party;
	struct species *species;
	struct occ *occ;
	bool is_clone;

	struct astar_node *path;	// Added when I rewrote party
	// rendezvous

        int hp_mod;
        int hp_mult;
        int mp_mod;
        int mp_mult;

        struct closure *ai; // experimental

 protected:
	bool initCommon(void);
	bool isAttackTargetInRange();
        void pathfind_to(class Object *target);
        void getAppointment();
        
	char *name;
	int hm;

        // -------------------------------------------------
        // Custom attribute modifiers
        // -------------------------------------------------


	int xp;
	int order;
	bool sleeping;
	int ac; // used by spells
	int armsIndex;
	int str;
	int intl;
	int dex;
	int mana;
	int lvl;
        int defenseBonus;
	bool playerControlled;
	bool solo;
	class ArmsType *currentArms;
	class Character *target;
	class ArmsType **rdyArms;
	bool fleeing;
	int fleeX, fleeY;
	int burden;
	bool inCombat;
        char *damage_sound;

	// *** NPC fields
	virtual void dropRdyArms();
	virtual bool dropItems();
	class Container *container;
	bool rearm;

	// *** PC fields
	struct sprite *sprite;

	int n_rest_credits;
	bool elevated;
        bool charmed;

        // --------------------------------------------------------------------
        // Stuff for resting/camping:
        // --------------------------------------------------------------------
        clock_alarm_t wakeup_alarm;
        clock_alarm_t rest_alarm;
        bool resting;
        bool guarding;

	struct sched *sched;
        int activity;
        int appt;

        bool is_leader;
        void (*ctrl)(class Character *character);

        int factionSwitch;
        int tmpFaction;

};

extern void char_dtor(void *val);
extern void char_save(struct save *save, void *val);

#endif
