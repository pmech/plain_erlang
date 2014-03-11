-module(literl).

-compile([export_all]).

main(Filename)->
    {ok, In} = file:open(Filename, [read]),
    {ok, Out} = file:open(Filename++".md", [write]),
    translate_lines(markdown, In, Out).

translate_lines(Mode, In, Out) ->
    case file:read_line(In) of
        {ok, Line} ->
            NewMode = translate_line(Mode, Line, Out),
            translate_lines(NewMode, In, Out);
        eof ->
            file:close(In),
            file:close(Out);
        Err ->
            exit(Err)
    end.

translate_line(Mode, Src_line, Out) ->
    case {Mode, line_type(re:run(Src_line, "^%+ *(.*)$|^ +$|^$",
                                 [{capture, all_but_first, binary}]))} of
        {markdown, {markdown, Line}} ->
            markdown(Out, Line),
            markdown;
        {code, code} ->
            code(Out, Src_line),
            code;
        {markdown, code} ->
            begin_code(Out),
            code(Out, Src_line),
            code;
        {code, {markdown, Line}} ->
            end_code(Out),
            markdown(Out, Line),
            markdown
    end.

line_type({match, Line}) ->
    io:format(">>>> ~p~n", [Line]),
    {markdown, [Line,<<"\n">>]};
line_type(nomatch) ->
    code.

code(Out, Line) ->
    write(Out, ["   ",Line]).
markdown(Out, Line) ->
    write(Out, Line).
begin_code(Out) ->
    write(Out, "```erlang\n").
end_code(Out) ->
    write(Out, "```\n").

write(Out, Line) ->
%    io:format("~s~n", [Line]),
    file:write(Out, Line).
