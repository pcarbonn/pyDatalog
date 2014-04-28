# coding=UTF-8
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
        
Two literal's tags are the same if there is a one-to-one
mapping of variables to variables, such that when the mapping is
applied to one literal, the result is a literal that is the same as
the other one, when compared using structural equality.  The
tag is used as a key by the subgoal table.

Term           id            tag
-------        --            ---
variable_1     (f,1)         (v,1)
X              (f,X)         (v,2)
1              1             1
1.0            1.0           1.0
'a'            'a'           'a'
('a', X)       ('a', (f,X))  ('a',(v,2))

X+1            ((f,X),+,1)   ((v,2),+,1)
lambda X:..    (None,<l>,(f,X))    (None,<l>,(v,2))
X[0]           (X,[,0)       ((v,2),[,0)
range_(N)      (None,..,(f,N))    (None,..,(v,2))
len_(X)        (None,#,(f,X))    (None,#,(v,2))



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


#aggregate ####################################################

in original source code : 
    (f[Key]==aggregate(Value, for_each=For_each, order_by=Order_by)) <= q(Key, Value, For_each, Order_by)

in knowledge base :
    (f[Key]==_pyD_X1) <= f!1°(Key, Value, Group_by, For_each, Order_by, _pyD_X1)
        where f!1° is a literal with aggregate attribute
              whose argument list has no repetition of variables
    f!1(Key, Value, Group_by, For_each, Order_by) <= q(Key, Value, Group_by, For_each, Order_by)
        where the argument list of f!1 has no repetition of variables

resolution algorithm for f!1°:
    drop the last term
    variabilize Key that are not in Group_by (for rank, running_sum aggregation)
    find all f!(Key, Value, Group_by, For_each, Order_by)
    group by Group_by
    sort by Order_by
    apply aggregate 
    return f!°(Key, "", "", "", "", _pyD_X1) solutions

"""