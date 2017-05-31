#pragma once

#include "closure.h"

// Note: if you change the layers you'll probably need to change the save
//       file
// Proper rendering depends on keeping these in order!
enum layer {
	null_layer = 0,
	tfeat_layer = 1,
	mech_layer = 2,
	portal_layer = 3,
	vehicle_layer = 4,
	bed_layer = 5,
	container_layer = 6,
	item_layer = 7,
	field_layer = 8,
	being_layer = 9,
	projectile_layer = 10,
	cursor_layer = 11
};


class ObjectType {

 public:
	virtual bool isType(int classID);
	virtual int getType();
	 ObjectType();
	 ObjectType(const char *tag, const char *name, struct sprite *sprite,
		    enum layer layer);
	 virtual ~ ObjectType();
	virtual bool init(char *tag, char *name, enum layer layer,
			  struct sprite *sprite);
	virtual void setSprite(struct sprite *sprite);
	virtual const char *getTag();
	virtual const char *getName();
	virtual struct sprite *getSprite();
	virtual enum layer getLayer();
	virtual class Object *createInstance();
	virtual bool isVisible();
	virtual void describe(Object * obj);
	virtual int getSpeed();
	virtual int getMaxHp();
	virtual void setMovementMode(struct mmode *mmode);
	virtual struct mmode *getMovementMode();

	// This might turn out to be too vague. We'll see.
	virtual int getRequiredActionPoints();

	// This version of describe() can't run any hooks; it's used for
	// inventory descriptions, where there are no object instances.
	void describeType(int count);

	bool isUsable();	// items, etc
	bool isReadyable();	// arms
	bool isMixable();	// reagents
	bool isCastable();	// spells
	bool canExec();		// mechs, etc
	bool canGet();
	bool canDrop();
	bool canOnReady();
	bool canOnUnready();
	bool canOpen();
	bool canStep();
	bool canHandle();
	bool canSense();
	bool canXamine();
	bool canAttack();
	bool canOnAttack();
	bool canEnter();
	bool canBump();		// attempted entry onto same tile
	bool canHitLocation();	// weapon hitting a target location
	bool canBuy();		// has a hook for 'buy'
	bool canSearch();	// has a hook for 'search'
	bool isQuestItem();

	// These return the result of closure_exec:
	int use(Object * user);
	int exec(Object * obj);
	int get(Object * obj, Object * getter);
	int drop(Object * obj, Object * dropper, struct place *, int, int);
	int onReady(Object *actor);
	int onUnready(Object *actor);
	int open(Object * obj, Object * opener);
	int step(Object * obj, Object * stepper);
	int sense(Object * obj, Object * stepper);
	int xamine(Object * obj, Object * xaminer);
	int handle(Object * obj, Object * handler);
	int attack(Object * obj, Object * attacker);
	int onAttack(Object * obj, Object * attacker);
	int enter(Object * obj, Object * enterer);
	int cast(Object * caster);
	int bump(Object * obj, Object * bumper);
	int hitLocation(Object * obj, Object * attacker, Object * target,
			struct place *place, int x, int y, int dam);
	int buy(Object * buyer, int q);
	int search(Object * obj, Object * searcher);

	closure_t *getGifc();
	void setGifc(closure_t * gifc, int cap);

	void setPluralName(char *val);
	void setGob(struct gob *gob);
	struct gob *getGob();
	void setQuestItemFlag(bool val);

 protected:
	char *tag;
	char *name;
	struct sprite *sprite;
	enum layer layer;
	int speed;
	int required_action_points;
	int max_hp;

	/* ghulscript-interface (gifc) */
	closure_t *gifc;
	int gifc_cap;
	struct gob *gob;
	struct mmode *movementMode;

 private:
	 bool hasDescribeHook();
	void runDescribeHook(Object * obj);

	char *pluralName;
	char *getPluralName();
	bool questItemFlag;
};

