# last
the universe rewritten in lisp. 
to run locally, use eval. function, with read and loop for convience.

- get and set, edit slots on the object itself.
- (fn... runs the fn function if it is defined on the object itself.
- to define a method, set a lambda list.
- create new objects with (create name)
- edit the object using (cal object (set/get/run ....

# vision
(create ball)
(cal ball (set bounce (lambda  () 
                          "You bounced the ball"
                           (set health (1- (get health))))))
(call ball bounce)
> "You bounced the ball"
