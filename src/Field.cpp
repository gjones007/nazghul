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

#include "Field.h"
#include "common.h"
#include "session.h"

bool FieldType::isType(int classID) 
{
        if (classID == FIELD_TYPE_ID)
                return true;
        return ObjectType::isType(classID);
}

int FieldType::getType()
{
        return FIELD_TYPE_ID;
}

FieldType::FieldType(char *tag, char *name, struct sprite *sprite, 
                     int light, int duration, int pclass, closure_t *clx)
        : ObjectType(tag, name, sprite, field_layer), 
          light(light), duration(duration), pclass(pclass), effect(clx)
{
}

FieldType::~FieldType()
{
        if (effect)
                closure_del(effect);
}

int  FieldType::getLight()
{
        return light;
}

void  FieldType::setLight(int val)
{
        light = val;
}

void  FieldType::setDuration(int val)
{
        duration = val;
}

int  FieldType::getDuration()
{
        return duration;
}

class Object *FieldType::createInstance()
{
        return new Field(this);
}

class FieldType * Field::getObjectType()
{
        return (class FieldType *) Object::getObjectType();
}

bool FieldType::isPermanent()
{
        return (duration < 0);
}

//////////////////////////////////////////////////////////////////////////////

Field::Field(FieldType *type)
        : Object(type)
{
        duration = type->getDuration();
}

Field::Field(FieldType *type, int dur)
        : Object(type)
{
        duration = dur;
}

Field::Field() : duration(0)
{        
}

Field::~ Field()
{
}

int Field::getLight()
{
        return getObjectType()->getLight();
}

void Field::exec(struct exec_context *context)
{
        startTurn();
        if (isDestroyed())
                return;
        
        if (getObjectType()->isPermanent())
                return;

        duration--;
        assert(duration >= 0);
        if (duration == 0)
                destroy();
}

void Field::save(struct save *save)
{
        save->write(save, "(kern-mk-field %s %d)", getObjectType()->getTag(), 
                    duration);
}

int Field::getPclass()
{
        return getObjectType()->pclass;
}
