haskell
=======

map operations
--------------
* monoidic operations on elements inserted into a container. 
	* for eg., Continuously perform aggregation of values upon changes to the container. Should be monoidic.

Grouping operations
-------------------
* chronological grouping based on a user defined date (for eg., expiry of trades)
* Multiple grouping parameters

Monad transformers
------------------
* Examples of Writer monad composed of a state monad which in turn is composed of a maybe monad
* Being able to decide the right order of composition based on the problem domain
