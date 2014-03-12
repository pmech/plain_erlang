
Simple Literate Programming compiler
------------------------------------

This is a basic erlang module that will translate markdown files in
the current dir to erl-files and then compile these.


### Module Setup

Defining the module.

    -module(literl).

Include file.hrl.  Contain the record definition for the output from
read_file_info, which is used below.

    -include_lib("kernel/include/file.hrl").

### Exports

This module only export one function: make.

    -export([make/0]).

### The main function

The make function finds all .erl.md files.  It then compiles these
into regular erlang code by prepending %% for all non-code lines in
the md files.  Last it uses the make module from the tools appliction
to compile the erlang files to beam.

    make() ->
      foreach_md_file(fun compile/1),
      make:all([load]).

Compile an markdown file if it is younger that its corresponding
Erlang-file.

    compile(Md_filename) ->
      Erl_filename = erl_filename(Md_filename),
      case younger_than(Md_filename, Erl_filename) of
        true  ->
          io:format("Compiling ~s > ~s~n",[Md_filename, Erl_filename]),
          {ok, Out} = file:open(Erl_filename, [write]),
          for_each_line_in_file(Md_filename,
                                fun(Line) -> compile_line(Line, Out) end),
          file:close(Out);
        false -> ok
      end.

Compiles a line from a markdown file.  Works by adding Erlang comments
(%%) in front of all non-code-block lines in the markdown file.

    compile_line(Line, Out) ->
      case code_block_line_p(Line) of
        true  -> file:write(Out, Line);
        false -> file:write(Out, comment_line(Line))
      end.

A code-block line is identified by not having a non-white-space
character as the first character in the line.

    code_block_line_p(Line) ->
      case re:run(Line, "^\\S") of
        {match, _} -> false;
        _          -> true
      end.

Commints a line, uses the IOData type.

    comment_line(Line) -> ["%%", Line].

Utility function that will apply a given function for each line an a
file.  It will both open the file for reading and close the file when
done.

    for_each_line_in_file(Filename, Fun) when is_list(Filename) ->
      {ok, Fd} = file:open(Filename, [read]),
      for_each_line_in_file(Fd, Fun);
    for_each_line_in_file(Fd, Fun) ->
      case file:read_line(Fd) of
        {ok, Line} ->
          Fun(Line),
          for_each_line_in_file(Fd, Fun);
        eof ->
          file:close(Fd),
          ok
      end.

### Helper functions

Remove the .md part from <filename.erl.md>

    erl_filename(Md_filename) ->
      string:substr(Md_filename, 1, length(Md_filename)-3).

Check if the file F1 is younger than F1

    younger_than(F1, F2) ->
      case {mtime(F1), mtime(F2)} of
        {{ok, T1}, {ok, T2}} ->
          T1 > T2;
        {nofile, {ok,_}} ->
          false;
        {{ok, _}, nofile} ->
          true
      end.

Find the last modified time for a file

    mtime(Filename) ->
      case file:read_file_info(Filename, [{time, posix}]) of
        {ok, Info} ->
          {ok, Info#file_info.mtime};
        {error, enoent} ->
          nofile
      end.

Apply Fun for each markdown file in the current dir

    foreach_md_file(Fun) ->
      lists:foreach(Fun, filelib:wildcard("*.erl.md")).
