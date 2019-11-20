# Prop generator architecture

## Data model

### World

Prop generates worlds in a form similar to 1NF normalized relational
models. Entities are maps of attributes to values, and relations are
modeled as values that reference unique attribute/value pairs.


```clojure
{:cat    #{{:cat/id    2
            :cat/owner [:person/id 1]
            :cat/name  "Pearl's cat"}} ; cat's name is generated from cat/owner's name
 :person #{{:person/id   1
            :person/name "Pearl"}}}
```

Attributes belong to a particular entity type, so we use qualified
keywords to indicate the entity and prevent name clashes.

## Attribute schema

Describes all attributes to be generated, with for each attibute a
generator and an optional list of direct dependencies.

```clojure
(def attributes #{{:name      :cat/id
                   :generator gen-uniq-id}
                  {:name      :cat/name
                   :generator (fn [state]
                                (str (-> (get-relation state :cat/owner) :person/name)
                                     "'s cat"))
                   :deps      [:cat/owner :person/name]}
                  {:name      :cat/owner
                   :generator (select-ref :person/id)
                   :deps      [:person/id]}
                  {:name      :person/id
                   :generator gen-uniq-id}
                  {:name      :person/name
                   :generator gen-name}})
```

## Population distribution

Describes the amount of entities of each type to generate:

```clojure
(def distribution {:cat    4
                   :person 3})
```

## Many-to-many relations

In *Prop*'s worldview, the basic mechanism to relate entities is trough
references. Many-to-many relationships are then modeled as
an intermediate entity type containing two refs, one for each side of
the relation:

```clojure
; A world where cats can have multiple people serving them
{:cat          #{{:cat/id   2
                  :cat/name "Cat served by Pearle and Barney"}}
 :cat-servants #{{:servant/lord  [:cat/id 2]
                  :servant/vasal [:person/id 1]}
                 {:servant/lord  [:cat/id 2]
                  :servant/vasal [:person/id 2]}}
 :person       #{{:person/id   1
                  :person/name "Pearl"}
                 {:person/id   2
                  :person/name "Barney"}}}
```

## Generating a world

### Sort attributes on dependencies

Aborts when cycles are detected.

### Generate shell entities

Generate empty entities.

### Generate properties

For each attribute in the sorted list, generate the attribute/value
pairs needed for the partially populated entities.

Generated are handed the state of the world being created, and so can
reference other attributes, in other entities if necessary.

## Strategies for generating values

### Basic generators

Generators take the world state as an argument and return a new
value. The value generated should depend only on the given arguments
and the state of the `clojure.data.generators/*gen*` Random instance.

### Constraints

Often it's difficult to write a function that provides exactly the
right set of values. In this case it's possible to constrain a
generator that provides "mostly" the right kind of values.

Constraints are implemented as predicates on generated
values and world state. When the property's value does not satisfy all
of its constraints, a new value will generated. If no valid value was
generated within `nl.surf.prop/*retries*` attempts, the whole world
generation is aborted -- no attempt is made to backtrack.

### Generating references

When no additional constraints are needed, a reference attribute just
needs a specified dependency on its subject, and a value is picked by
random selection of all subject entities. The distribution of
references and subject values is then driven only by the cardinalities
of the related entity types.

When the relation needs to be unique, a selection can be made without
replacement (pick randomly from all non-taken values).

To drive selections towards typical cases, we can use the "power of
two random choices"; select two references using one of the above
strategies and pick the one preferred by a given comparator or
predicate -- for instance, preferring the least-used choice.

## Consequences of design

### Manually specify dependencies

The configuration must specify the correct dependencies used by the
generators. If you get this wrong, the attributes may be generated in
the wrong order meaning you'll can end up using "unpopulated"
properties.

### Manual balancing of entity population

There is no way to directly specify things like "there should be
between 2 and 5 lecturers for every course" (typical counts on
many-to-many relationships).

The user will have to specify a workable ratio of entities (including
amounts of relationship entities) and can then nudge the typical case
using a predicate as mentioned in ["generating references"](#generating-references).

### Composite unique attributes (unique many-to-many relations)

Since attributes are selected one-by-one, it becomes difficult to generate
unique combined properties, like unique many-to-many relation
entities.

TODO: hier iets over cominaties afleiden van 1 "hidden" attribuut,
zodat ze in 1x gegenereerd kunnen worden.

### One-shot generation

Properties are generated in relative isolation; depende

### Configuration

This design constraints what can be usefully configured about the
worlds to be be generated; it becomes difficult to 

### Depletion


