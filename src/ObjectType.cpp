#include "ObjectType.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "gob.h"
#include "log.h"
#include "Object.h"

ObjectType::ObjectType()
{
	assert(false);
}

ObjectType::ObjectType(const char *tag, const char *sname,
		       struct sprite * sprite_, enum layer layer_)
 : sprite(sprite_), layer(layer_), speed(0), required_action_points(0),
max_hp(0), gifc(NULL), gifc_cap(0), gob(NULL), pluralName(NULL)
{
	this->tag = tag ? strdup(tag) : NULL;
	this->name = sname ? strdup(sname) : NULL;
}

ObjectType::~ObjectType()
{
	if (tag)
		free(tag);
	if (name)
		free(name);
	if (gifc)
		closure_unref(gifc);
	if (pluralName)
		free(pluralName);
	if (gob)
		gob_unref(gob);
}

void ObjectType::setPluralName(char *val)
{
	if (pluralName)
		free(pluralName);
	if (val)
		pluralName = strdup(val);
	else
		pluralName = NULL;
}

char *ObjectType::getPluralName()
{
	return pluralName;
}

bool ObjectType::init(char *tag, char *name, enum layer layer,
		      struct sprite * sprite)
{
	this->tag = tag ? strdup(tag) : NULL;
	this->name = name ? strdup(name) : NULL;
	this->sprite = sprite;
	this->layer = layer;
	return (this->tag != 0 && this->name != 0);
}

class Object *ObjectType::createInstance()
{
	return new Object(this);
}

void ObjectType::setSprite(struct sprite *sprite)
{
	this->sprite = sprite;
}

/* FIXME - move to a string utility file */
static int endsWith(const char *word, const char *end)
{
	int wlen = strlen(word) - 1;
	int elen = strlen(end) - 1;

	if (wlen < elen)
		return 0;

	while (elen >= 0) {
		if (word[wlen--] != end[elen--])
			return 0;
	}

	return 1;
}

void ObjectType::describeType(int count)
{
	if (1 == count) {
		if (isvowel(name[0]))
			log_continue("an ");
		else
			log_continue("a ");
		log_continue("%s", getName());
	} else if (getPluralName()) {
		log_continue("some %s (%d)", getPluralName(), count);
	} else {
		if (endsWith(name, "s")
		    || endsWith(name, "sh"))
			log_continue("some %ses (%d)", getName(), count);
		else
			log_continue("some %ss (%d)", getName(), count);
	}
}

void ObjectType::describe(Object * obj)
{
	if (hasDescribeHook()) {
		runDescribeHook(obj);
		return;
	}

	describeType(obj->getCount());
}

bool ObjectType::isType(int classID)
{
	return (classID == OBJECT_TYPE_ID);
}

int ObjectType::getType()
{
	return OBJECT_TYPE_ID;
}

const char *ObjectType::getTag()
{
	return tag;
}

const char *ObjectType::getName()
{
	return name;
}

struct sprite *ObjectType::getSprite()
{
	return sprite;
}

enum layer ObjectType::getLayer()
{
	return layer;
}

bool ObjectType::isVisible()
{
	return true;
}

int ObjectType::getSpeed()
{
	return speed;
}

int ObjectType::getRequiredActionPoints()
{
	return required_action_points;
}

int ObjectType::getMaxHp()
{
	return max_hp;
}
