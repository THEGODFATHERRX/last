# last
the universe rewritten in lisp. 
to run locally, use the repl function, which logs in as GOD.

- get and set, edit slots on the object itself.
- (func-name... runs the function if it is defined on the object itself.
- to define a method, set a lambda list.
- create new objects with (create name)
- edit the object using (cal object (set/get/run ....

# vision
```lisp
(create ball)
(cal ball (set bounce (lambda  () 
                          "You bounced the ball"
                           (set health (1- (get health))))))
(call ball bounce)
;> "You bounced the ball"
```
