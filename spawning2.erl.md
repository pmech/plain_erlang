
This is a test for Literate Programming
=======================================

Module setup
------------

First the module is defined

    -module(spawning2).

### Some compiler settings

All functions are exported, this makes it easier to test if stuff is
working

    -compile([export_all]).


The functions
--------------

The first test function

    test() ->
        %% Just return ok
        ok.

EOF
