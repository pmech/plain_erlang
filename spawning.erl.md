
This is a test for Literate Programming
=======================================

## Module setup #######################

First the module is defined
```erlang
   -module(spawning).
```

All functions are exported, this makes it easier to test if stuff
is working
```erlang
   -compile([export_all]).
```

## The functions

The first test function
```erlang
   test() ->
       %% Just return ok
       ok.
```

EOF
