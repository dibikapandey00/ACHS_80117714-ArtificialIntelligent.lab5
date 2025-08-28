#Design and implement a simple frame-based knowledge representation system in Prolog using the concept of classes, slots, inheritance, and instances.Your system should allow defining classes with default attributes (slots), creating instances of classes, overriding slot values for specific instances, and retrieving slot values with inheritance.

:- dynamic class/3.
:- dynamic instance/2.
:- dynamic slot_value/3.

define_class(ClassName, Parent, Slots) :-
    assertz(class(ClassName, Parent, Slots)).

create_instance(InstanceName, ClassName) :-
    assertz(instance(InstanceName, ClassName)),
    class(ClassName, _, Slots),
    initialize_slots(InstanceName, Slots).

initialize_slots(_, []).
initialize_slots(InstanceName, [slot(Name, Default)|Rest]) :-
    assertz(slot_value(InstanceName, Name, Default)),
    initialize_slots(InstanceName, Rest).

set_slot(Instance, Slot, Value) :-
    retractall(slot_value(Instance, Slot, _)),
    assertz(slot_value(Instance, Slot, Value)).

get_slot(Instance, Slot, Value) :-
    instance(Instance, Class),
    get_slot_inheritance(Class, Instance, Slot, Value).

get_slot_inheritance(_, Instance, Slot, Value) :-
    slot_value(Instance, Slot, Value), !.
get_slot_inheritance(Class, _, Slot, Value) :-
    class(Class, _, Slots),
    member(slot(Slot, Default), Slots),
    Value = Default, !.
get_slot_inheritance(Class, Instance, Slot, Value) :-
    class(Class, Parent, _),
    Parent \= nil,
    get_slot_inheritance(Parent, Instance, Slot, Value).

class_exists(ClassName) :-
    class(ClassName, _, _).

instance_exists(InstanceName) :-
    instance(InstanceName, _).

all_slots(ClassName, AllSlots) :-
    class_chain(ClassName, Chain),
    collect_slots(Chain, AllSlots).

class_chain(ClassName, [ClassName|Rest]) :-
    class(ClassName, Parent, _),
    Parent \= nil,
    class_chain(Parent, Rest).
class_chain(ClassName, [ClassName]) :-
    class(ClassName, nil, _).

collect_slots([], []).
collect_slots([Class|Rest], AllSlots) :-
    class(Class, _, Slots),
    collect_slots(Rest, RestSlots),
    append(Slots, RestSlots, AllSlots).