"""
pyDatalog

Copyright (C) 2013 Pierre Carbonnelle

This library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc.  51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA


#hash comments ################################################## 

This file documents functionality implemented in code occurring in
various places.  It is divided in chapter whose title begin with a
hash (e.g. #repr).  This hash has also been added to the source code
implementing functionality.  Most source code editors will allow you 
to search for occurrences of this hash title throughout the source 
files.


#id  identifiers ################################################

The id's encoding ensures that two literals are structurally the
same (up to prearity terms) if they have the same id. 
Prearity is used to ensure unicity of results of functions like "pred[k]=v"
        
Two literal's variant tags are the same if there is a one-to-one
mapping of variables to variables, such that when the mapping is
applied to one literal, the result is a literal that is the same as
the other one, when compared using structural equality.  The
variant tag is used as a key by the subgoal table.

Element        id            key            tag
-------        --            ---            ---
variable_1     variable_1    f1             v1
X              X             v2             v2
1              1             c2             c3
1.0            1.0           c4             c4
'a'            'a'           c5             c5
('a', X)       ('a', X)      (c5v2)         (c5v1)
X+1            (X+1)         (v2+c1)        (v2+c1)
lambda X:..    (c1l1v2)      (c1l1v2)       (c1l1v2)
X[0]           (X[0)         (v2[c2)        (v2[c2)
range_(N)      (c1..v2)      (c1..v2)       (c1..v1)
len_(X)        (c1#v2)       (c1#v2)        (c1#v1)
p              p/2
p(X,1)         p/2v2c3       p/2v2c3        p/2v1c3
p[X]==1        p/2v2c3       p/2v2c3        p/2v1
X.b            (X.c1)        (X.c1)         (X.c1)
X.m(Y)         ((X.c1)(Y)    ((v1.c1)(v2)   ((v1.c1)(v2)
A.p[X]==1     see #comparison



#comparison prefixed literal #######################

A term is pre-appended to expression of the form P[X]==Y, P.s[X]==Y, and P.s(X).

The added term represents P.  
When P is a class, the extra term is a constant string '_pyD_class'
When P is a variable, the extra term is subject to substitution 
in the search algorithm, so that its bound value can be used 
to evaluate the expression.

When calling python resolvers, this term is removed, 
and added back to the result received.

In aggregate predicate, this term also needs special care.

TODO : do no add this term for P[X]==Y (beware of aggregates, though)


#unify unify and substitution ###################################

An environment is a map from variables to terms.

Unify results either in an environment or None.
None is returned when the two literals cannot be unified.  When they
can, applying the substitutions defined by the environment on both
literals will create two literals that are structurally equal (i.e.
have the same id).

Chase applies the substitution.

The case analysis for unifying two terms is handled by method
dispatch.


#shuffle    shuffle and rename ##################################
 
Shuffle creates an environment in which all variables are mapped to
freshly generated variables.

It is used to "rename" a literal.



#pred  predicate.name ###########################################

A name has one of these formats:
- an alphanumeric string
- an alphanumeric string, a '.' and an alphanumeric string (prefixed)

A predicate name has one of these formats
- a name
- '~' followed by a name (negated literal)
- the name of an aggregate function followed by '==!' (e.g. min==!)
- a comparison operator (==, <, ...)
- a name followed by a comparison operator (function comparison)

"""