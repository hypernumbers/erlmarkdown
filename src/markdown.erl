%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Gordon Guthrie
%%% @docws,
%%%
%%% @end
%%% Created : 10 Sep 2009 by gordonguthrie@backawinner.gg
%%%-------------------------------------------------------------------

-module(markdown).

-export([conv/1]).

-import(lists, [flatten/1, reverse/1]).

-include_lib("eunit/include/eunit.hrl").

-define(SPACE, 32).
-define(TAB, 9).
-define(LF, 10).
-define(CR, 13).
-define(NBSP, 160).

%%% the lexer first lexes the input
%%% make_lines does 2 passes:
%%% * it chops the lexed strings into lines which it represents as a
%%%   list of lists
%%% * it then types the lines into the following:
%%% * normal lines
%%% * reference style links
%%% * reference style images
%%% * special line types
%%%   - blank
%%%   - SETEXT header lines
%%%   - ATX header lines
%%%   - blockquote
%%%   - unordered lists
%%%   - ordered lists
%%%   - code blocks
%%%   - horizontal rules
%%% the parser then does its magic interpolating the references as appropriate
conv(String) -> io:format("String is ~p~n", [String]),
                Lex = lex(String),
                io:format("Lex is ~p~n", [Lex]),
                UntypedLines = make_lines(Lex),
                io:format("UntypedLines are ~p~n", [UntypedLines]),
                TypedLines = type_lines(UntypedLines),
                io:format("TypedLines are ~p~n", [TypedLines]),
                Ret = parse(TypedLines),
                io:format("Ret is ~p~n", [Ret]),
                Ret.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Parse the lines
%%% This is a two part parser
%%% * first it extracts referenced links and images
%%% * then it processes the lines, interpolating the references
%%%   as a appropriate
%%%
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse({UrlRefs, ImgRefs, TypedLines}) ->
    p1(TypedLines, UrlRefs, ImgRefs, [], []).

%% goes through the lines
%% If it hits a 'start' like 'blockquote' it throws a HTML fragment into the mix
%% (eg <blockquote>) and then the matching closing one onto the stack (eg </blockquote>)

%% when it hits a hard return it then throws the matching closing one back in...
p1([], _U, _I, [], Acc)    -> flatten(reverse(Acc)); 
p1([], _U, _I, Stack, Acc) -> flatten(reverse([reverse(Stack) | Acc]));

%% Tags have the highest precedence...
p1([{tag, Tag} | T], U, I, Stack, Acc) ->
    case T of
        [{blank, _} | T2] -> p1(T2, U, I, Stack,
                                [make_tag_str(Tag) | Acc]);
        _Other            -> p1(T, U, I, Stack,
                                ["<p>" ++ make_str(Tag, U, I) ++ "</p>" | Acc])
    end;
%% These clauses handle state issues
%% hit a blank line - you close the last open state (if it exists) and
%% throw a <br />
p1([{blank, _} | T], U, I, [], Acc) ->
    p1(T, U, I, [], ["<br />" | Acc]); 
p1([{blank, _} | T], U, I, [Close | Open], Acc) ->
    p1(T, U, I, Open, flatten(["<br />", Close | Acc]));

%% two consecutive normal lines should be concatenated..
p1([{normal, P1}, {normal, P2} | T], U, I, Stack, Acc) ->
    p1([{normal, flatten([P1 | P2])} | T], U, I, Stack, Acc);

%% setext h1 is a look behind
p1([{normal, P}, {setext_h1, _} | T], U, I, Stack, Acc) ->
    p1(T, U, I, Stack, ["<h1>" ++ make_str(snip(P), U, I) ++ "</h1>" | Acc]); 

%% setext h2 might be a look behind
p1([{normal, P}, {h2_or_hr, _} | T], U, I, Stack, Acc) ->
    p1(T, U, I, Stack, ["<h2>" ++ make_str(snip(P), U, I) ++ "</h2>" | Acc]); 

%% blockquote is look ahead push blockquote onto the state
p1([{blockquote, _P} | T], U, I, Stack, Acc) ->
    p1(T, U, I, ["</blockquote>" | Stack], ["<blockquote>" | Acc]);

%% but one is just normal...
p1([{normal, P} | T], U, I, Stack, Acc) ->
    p1(T, U, I, Stack, ["<p>" ++ make_str(snip(P), U, I) ++ "</p>" | Acc]);

%% atx headings
p1([{{h1, P}, _} | T], U, I, Stack, Acc) ->
    NewP = string:strip(make_str(P, U, I), right),
    p1(T, U, I, Stack, ["<h1>" ++ NewP ++ "</h1>" | Acc]); 
p1([{{h2, P}, _} | T], U, I, Stack, Acc) ->
    NewP = string:strip(make_str(P, U, I), right),
    p1(T, U, I, Stack, ["<h2>" ++ NewP ++ "</h2>" | Acc]); 
p1([{{h3, P}, _} | T], U, I, Stack, Acc) ->
    NewP = string:strip(make_str(P, U, I), right),
    p1(T, U, I, Stack, ["<h3>" ++ NewP ++ "</h3>" | Acc]); 
p1([{{h4, P}, _} | T], U, I, Stack, Acc) ->
    NewP = string:strip(make_str(P, U, I), right),
    p1(T, U, I, Stack, ["<h4>" ++ NewP ++ "</h4>" | Acc]); 
p1([{{h5, P}, _} | T], U, I, Stack, Acc) ->
    NewP = string:strip(make_str(P, U, I), right),
    p1(T, U, I, Stack, ["<h5>" ++ NewP ++ "</h5>" | Acc]); 
p1([{{h6, P}, _} | T], U, I, Stack, Acc) ->
    NewP = string:strip(make_str(P, U, I), right),
    p1(T, U, I, Stack, ["<h6>" ++ NewP ++ "</h6>" | Acc]); 

%% unordered lists
p1([{{ul, _P}, _} | _T] = List, U, I, Stack, Acc) ->
    {Rest, NewAcc} = parse_list(ul, List, U, I, []),
    p1(Rest, U, I, Stack, ["<ul>" ++ NewAcc ++ "</ul>" | Acc]);

%% unordered lists
p1([{{ol, _P}, _} | _T] = List, U, I, Stack, Acc) ->
    {Rest, NewAcc} = parse_list(ol, List, U, I, []),
    p1(Rest, U, I, Stack, ["<ol>" ++ NewAcc ++ "</ol>" | Acc]);

%% codeblock
p1([{{codeblock, P}, _} | T], U, I, Stack, Acc) ->
    p1(T, U, I, Stack, ["<pre><code>" ++ make_str(P, U, I)
                        ++ "</code></pre>" | Acc]);

%% horizontal rules
p1([{hr, _} | T], U, I, Stack, Acc) ->
    p1(T, U, I, Stack, ["<hr />" | Acc]);
%% the clause with a normal before an 'h2_or_hr' has already been
%% handled further up the tree, so this is a bona fide 'hr'...
p1([{h2_or_hr, _} | T], U, I, Stack, Acc) ->
    p1(T, U, I, Stack, ["<hr />" | Acc]); 

%% line breaks - and that's us
p1([{br, P} | T], U, I, Stack, Acc) ->
    p1(T, U, I, Stack, [P | Acc]).

%% this is a bit messy because of the way that hard lines are treated...
%% If your li's have a blank line between them the item gets wrapped in a para,
%% if not, they don't
parse_list(_Type, [], _U, _I, A) ->
    {[], reverse(A)};
%% if the '<li>' is followed by a blank then wrap it, throw the blank
%% back onto the tail
parse_list(Type, [{{Type, P}, _}, {blank, _} = B | T], U, I, A) ->
     parse_list(Type, [B | T], U, I, ["<li><p>" ++ make_esc_str(P, U, I)
                                      ++ "</p></li>" | A]);
%% this is the partner of the previous line...
parse_list(Type, [{blank, _}, {{Type, P}, _} | T], U, I, A) ->
    {Rest, NewP} = grab(T, U, I, []),
    parse_list(Type, Rest, U, I, ["<li><p>" ++ make_esc_str(P, U, I)
                                  ++ NewP ++"</p></li>" | A]);
%% this is a plain old '<li>'
parse_list(Type, [{{Type, P}, _} | T], U, I, A) ->
    {Rest, NewP} = grab(T, U, I, []),
    parse_list(Type, Rest, U, I, ["<li>" ++ make_esc_str(P, U, I)
                                  ++ NewP ++ "</li>" | A]);
parse_list(_Type, List, _U, _I, A) ->
    {List, reverse(A)}.

%% grab grabs normals and double codeblocks
grab([{{codeblock, _}, S} | T] = List, U, I, Acc) ->
    case is_blockquote(S, T) of
        {{true, R1}, T2}       -> grab(T2, U, I,
                                       ["</blockquote>",
                                        make_esc_str(R1, U, I),
                                        "<blockquote>" | Acc]);
        {{esc_false, R1}, _T2} -> {R1, reverse(Acc)};
        {false, T2}            -> 
            case is_double_indent(S) of  % try codeblock
                false      -> {List, reverse(Acc)};
                {true, R2} ->
                    grab(T2, U, I, [make_esc_str(R2, U, I), "<br />" | Acc])
            end
    end;
grab([{normal, P} | T], U, I, Acc) ->
    grab(T, U, I, [make_esc_str(P, U, I) | Acc]);
grab(List, _U, _I, Acc) -> {List, reverse(Acc)}.

is_double_indent(List) -> is_double_indent1(List, 0).

%% double indent is any combination of tabs and spaces that add
%% up to 8
is_double_indent1([], _N)                  -> false;
is_double_indent1(Rest, N) when N > 7      -> {true, Rest};
is_double_indent1([{{ws, sp}, _} | T], N)  -> is_double_indent1(T, N + 1);
is_double_indent1([{{ws, tab}, _} | T], N) -> is_double_indent1(T, N + 4);
is_double_indent1(_List, _N)               -> false.

is_blockquote(List, T) ->
    case is_bq1(List, 0) of
        false          -> {false, T};
        {esc_false, R} -> {{esc_false, R}, T};
        {true, R}      -> {NewT, NewR} = grab2(T, R),
                          {{true, NewR}, NewT}
    end.
        
is_bq1([], _N)                            -> false;
is_bq1([{{ws, sp}, _} | T], N)            -> is_bq1(T, N + 1);
is_bq1([{{ws, tab}, _} | T], N)           -> is_bq1(T, N + 4);
is_bq1([{{md, gt}, _},
        {{ws, _}, _} | T], N) when N > 3  -> {true, T};
is_bq1([{{punc, backslash}, _},
        {{md, gt}, GT},
        {{ws, _}, WS} | T], N) when N > 3 -> {esc_false, [GT, WS | T]};
is_bq1(_List, _N)                         -> false.

grab2(List, R) -> gb2(List, reverse(R)).

gb2([], Acc)               -> {[], flatten(reverse(Acc))};
gb2([{blank, _} | T], Acc) -> {T, flatten(reverse(Acc))};
gb2([{_Type, P} | T], Acc) -> gb2(T, [P | Acc]).
             
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Make the lines from the raw tokens
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_lines(Tokens) -> ml1(Tokens, [], []).

ml1([], [], A2)                            -> reverse(A2);
ml1([], A1, A2)                            -> ml1([], [], [reverse(A1) | A2]);
%% this clause forces a <BR /> line before a blockquote...
ml1([{{ws, _}, _},
     {{ws, _}, _},
     {{lf, _}, _} = LF,
     {{md, gt}, _} = BQ,
     {{ws, _}, _} | T], A1, A2)      -> ml1(T, [], [[BQ], [{br, "<br />"}],
                                                    ml2(LF, A1) | A2]);
%% this is a normal blockquote
ml1([{{lf, _}, _} = LF,
     {{md, gt}, _} = BQ,
     {{ws, _}, _} | T], A1, A2) -> ml1(T, [], [[BQ], ml2(LF, A1) | A2]);
%% this clause forces a <BR /> line...
ml1([{{ws, _}, _},
     {{ws, _}, _},
     {{lf, _}, _} = H | []], A1, A2) -> ml1([], [], [[{br, "<br />"}],
                                                     ml2(H, A1) | A2]);
%% this clause generates a blockquote...
ml1([{{lf, _}, _} = H | T], A1, A2)  -> ml1(T, [], [ml2(H, A1) | A2]);
ml1([H | T], A1, A2)                       -> ml1(T, [H | A1], A2).

ml2(H, List) -> reverse([H | List]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Process the lines and give each line a type. The valid types are:
%%% * normal line
%%% * reference style links
%%% * reference style images
%%% * special line types
%%%   - blank
%%%   - SETEXT header lines
%%%   - ATX header lines
%%%   - unordered lists (including code blocks)
%%%   - ordered lists (including code blocks)
%%%   - blockquotes
%%%   - code blocks
%%%   - horizontal rules
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
type_lines(Lines) ->
    {Urls, Imgs, TypedLines} = t_l1(Lines, [], [], []),
    {Urls, Imgs, TypedLines}.

t_l1([], A1, A2, A3) -> {A1, A2, reverse(A3)};

%% this clause extracts URL and Image refs
%% (it is the only one that uses A1 and A2...
t_l1([[{{inline, open}, _} | T1] = H | T2], A1, A2, A3) ->
    {NewA1, NewA2} = case snip_ref(T1) of
                         {Id, {url, Url}} -> {[{Id, Url} | A1], A2};
                         {Id, {img, Img}} -> {A1, [{Id, Img} | A2]};
                         _Other           -> {A1, A2}
                     end,
    t_l1(T2, NewA1, NewA2, [{inlineref, H} | A3]);

%% types a blank line or a code block
t_l1([[{{lf, _}, _}| []]  = H | T], A1, A2, A3) ->
    t_l1(T, A1, A2, [{blank, H} | A3]);
t_l1([[{{ws, _}, _} | _T1] = H | T], A1, A2, A3) ->
    t_l1(T, A1, A2, [type_ws(H) | A3]);

%% types setext lines
t_l1([[{{md, eq}, _} | _T] = H | T], A1, A2, A3) ->
    t_l1(T, A1, A2, [type_setext_h1(H) | A3]);
%% NOTE 1: generates a ul as the default not a normal line
%% NOTE 2: depending on the context this might generate an <h2> header or an <hr />
t_l1([[{{md, dash}, _} | _T] = H | T], A1, A2, A3) ->
    t_l1(T, A1, A2, [type_setext_h2(H) | A3]);

%% types atx lines
t_l1([[{{md, atx}, _} | _T] = H | T], A1, A2, A3) ->
    t_l1(T, A1, A2, [type_atx(H) | A3]);
 
%% types blockquotes
t_l1([[{{md, gt}, _} | []] = H | T], A1, A2, A3) ->
     t_l1(T, A1, A2, [{blockquote, H} | A3]);

%% types unordered lists lines
%% NOTE 1: the dashed version is generated in type_setext_h2
%% NOTE 2: the asterix version also might generate a horizontal rule
%%         which is why it jumps to type_star2 <-- note the 2!!
t_l1([[{{md, star}, _}, {{ws, _}, _} | _T1] = H | T], A1, A2, A3) ->
    t_l1(T, A1, A2, [{type_star2(H), H} | A3]);
t_l1([[{{md, plus}, _}, {{ws, _}, _} = W | T1] = H | T], A1, A2, A3) ->
    t_l1(T, A1, A2, [{{ul, make_list_str([W | T1])}, H} | A3]);

%% types ordered lists...
t_l1([[{num, _} | _T] = H | T], A1, A2, A3) ->
    t_l1(T, A1, A2, [type_ol(H) | A3]);

%% types horizontal rules for stars and underscores
%% dashes and some stars are done elsewhere...
t_l1([[{{md, underscore}, _} | _T1] = H | T], A1, A2, A3) ->
    t_l1(T, A1, A2, [type_underscore(H) | A3]);
t_l1([[{{md, star}, _} | _T1] = H | T], A1, A2, A3) ->
    t_l1(T, A1, A2, [type_star(H) | A3]);

%% <BR /> typing
t_l1([[{br, P}] | T], A1, A2, A3) ->
    t_l1(T, A1, A2, [{br, P} | A3]);

%% Block level tags - these are look ahead they must be
%% on a single line (ie directly followed by a lf and nothing else
t_l1([[{{{tag, _Type}, Tag}, _} = H | T1] = List | T], A1, A2, A3) ->
    case is_blank(T1) of
        false -> t_l1(T, A1, A2, [{normal , List} | A3]);
        true  -> case is_block_tag(Tag) of
                     true  -> t_l1(T, A1, A2, [{tag , H} | A3]);
                     false -> t_l1(T, A1, A2, [{normal , List} | A3])
                 end
    end;

%% Final clause
t_l1([H | T], A1, A2, A3) ->
    t_l1(T, A1, A2, [{normal , H} | A3]).

%%
%% Loads of type rules...
%%

is_blank([])                        -> true;
is_blank([{{lf, _}, _} | []]) -> true;
is_blank([{{ws, _}, _} | T])        -> is_blank(T);
is_blank(_List)                     -> false.

is_block_tag("ADDRESS")    -> true;
is_block_tag("BLOCKQUOTE") -> true;
is_block_tag("CENTER")     -> true;
is_block_tag("DIR")        -> true;
is_block_tag("DIV")        -> true;
is_block_tag("DL")         -> true;
is_block_tag("FIELDSET")   -> true;
is_block_tag("FORM")       -> true;
is_block_tag("H1")         -> true;
is_block_tag("H2")         -> true;
is_block_tag("H3")         -> true;
is_block_tag("H4")         -> true;
is_block_tag("H5")         -> true;
is_block_tag("H6")         -> true;
is_block_tag("HR")         -> true;
is_block_tag("ISINDEX")    -> true;
is_block_tag("MENU")       -> true;
is_block_tag("NOFRAMES")   -> true;
is_block_tag("NOSCRIPT")   -> true;
is_block_tag("OL")         -> true;
is_block_tag("P")          -> true;
is_block_tag("PRE")        -> true;
is_block_tag("TABLE")      -> true;
is_block_tag("UL")         -> true;
is_block_tag(_Other)       -> false.

type_underscore(List) -> case type_underscore1(trim_right(List)) of
                             hr    -> {hr, List};
                             maybe -> {type_underscore2(List), List}
                         end.

type_underscore1([])                          -> hr;
type_underscore1([{{md, underscore}, _} | T]) -> type_underscore1(T);
type_underscore1(_List)                       -> maybe.

type_underscore2(List) -> case trim_right(List) of % be permissive of trailing spaces
                       [{{md, underscore}, _}, {{ws, _}, _},
                        {{md, underscore}, _}, {{ws, _}, _},
                        {{md, underscore}, _}]               -> hr;
                       _Other                                -> normal
                   end.

type_star(List) -> Trim = trim_right(List),
                   case type_star1(Trim) of % be permssive of trailing spaces
                       hr    -> {hr, trim_right(Trim)};
                       maybe -> {type_star2(List), List}
                   end.

type_star1([])                    -> hr;
type_star1([{{md, star}, _} | T]) -> type_star1(T);
type_star1(_List)                 -> maybe.

type_star2(List) ->
    case trim_right(List) of
        [{{md, star}, _}, {{ws, _}, _},
         {{md, star}, _}, {{ws, _}, _},
         {{md, star}, _}]                -> hr;
        _Other ->
            case List of
                [{{md, star}, _},
                 {{ws, _}, _}= WS | T] -> {ul, make_list_str([WS | T])};
                _Other2                -> normal
            end
    end.

type_ol(List) ->
    case type_ol1(List, []) of
        normal            -> {normal, List};
        {ol, Str}         -> {{ol, Str}, List};
        {esc_normal, Str} -> {normal, Str}
    end.
                         

%% this line terminates on an escaped fullstop after a number
%% (but you need to drop the backslash...)
type_ol1([{num, _} = N,
          {{punc, backslash}, _},
          {{punc, fullstop}, _} = P | T], Acc) ->
    {esc_normal, flatten([reverse(Acc), N, P | T])};
%% we accumulate the digits in case we need to escape a full stop in a normal line
type_ol1([{num, _} = H | T], Acc)  -> type_ol1(T, [H | Acc]);
type_ol1([{{punc, fullstop}, _},
          {{ws, _}, _} | T], _Acc) -> {ol, T};
type_ol1(_List, _Acc)              -> normal.

%% strip trailing #'s as they are decorative only...
type_atx(List) -> Stripped = reverse(strip_atx(reverse(List))),
                  {type_atx1(Stripped), List}.

strip_atx([{{md, atx}, _} | T]) -> strip_atx(T);
strip_atx(List)                 -> List.

%% this is final clause terminal
%% can't get a <h3> style header bigger than 6...
type_atx1(List) -> t_atx1(List, 0).

t_atx1([{{md, atx}, _} | T], 6) -> t_atx1(T, 6);
t_atx1([{{md, atx}, _} | T], N) -> t_atx1(T, N + 1);
t_atx1([{{ws, _}, _} | T], N)   -> t_atx1(T, N);
t_atx1(List, N)                 -> Nh = "h" ++ integer_to_list(N),
                                      Header = list_to_atom(Nh),
                                      {Header, List}.
                 
type_setext_h1(List) -> type_s_h1_1(List, []).

type_s_h1_1([], Acc)                      -> {setext_h1, reverse(Acc)};
type_s_h1_1([[] | T], Acc)                -> type_s_h1_1(T, Acc);
type_s_h1_1([{{md, eq}, _} = H | T], Acc) -> type_s_h1_1(T, [H | Acc]);
type_s_h1_1(L, Acc) ->
    {normal, [flatten(reverse(Acc)) | L]}.

type_setext_h2(List) ->
    case type_s_h2_1(List) of
        h2_or_hr -> {h2_or_hr, List};
        not_h2   -> {type_s_h2_2(trim_right(List)), List}
    end.                            

type_s_h2_1([])                    -> h2_or_hr;
type_s_h2_1([[] | T])              -> type_s_h2_1(T);
type_s_h2_1([{{md, dash}, _} | T]) -> type_s_h2_1(T);
type_s_h2_1(_L)                    -> not_h2.

type_s_h2_2([{{md, dash}, _}, {{ws,_}, _},
             {{md, dash}, _}, {{ws, _}, _},
             {{md, dash}, _}])              -> hr;
type_s_h2_2([{{md, dash}, _},
             {{ws, _}, _} = WS | T])        -> {ul, make_list_str([WS | T])};
type_s_h2_2(_List)                          -> normal.
 
type_ws(List) ->
    case type_ws1(List) of
        blank         -> {blank, List};
        try_codeblock ->
            case type_ws2(List) of
                normal           -> {normal, List};
                {codeblock, Ret} -> {{codeblock, Ret}, List}
            end
    end.

type_ws1([])                        -> blank;
type_ws1([{{lf, _}, _} | []]) -> blank;
type_ws1([[] | T])                  -> type_ws1(T);
type_ws1([{{ws, _}, _} | T])        -> type_ws1(T);
type_ws1(_L)                        -> try_codeblock.

type_ws2(List) -> t_ws2(List, 0).

%% 4 or more spaces takes you over the limit
%% (a tab is 4...)
t_ws2([{{ws, tab}, _} | T], _N) -> {codeblock, T};
t_ws2(List, N) when N > 3       -> {codeblock, List};
t_ws2([{{ws, sp}, _} | T], N)   -> t_ws2(T, N + 1);
t_ws2(_List, _N)                -> normal.

%% make a tag into a string
make_tag_str({{{tag, Type}, Tag}, _}) ->
    case Type of
        open         ->  "<" ++ Tag ++ ">";
        close        -> "</" ++ Tag ++ ">";
        self_closing ->  "<" ++ Tag ++ " />"
    end.

%% if it is a list we need to discard the initial white space...
make_list_str([{{ws, _}, _} | T] = List) ->
    case is_double_indent(List) of
        false     -> T;
        {true, R} -> flatten([{tags, "<pre><code>"} ,R ,
                              {tags, "</code></pre>"} | []])
    end.

%% All ref processing can ignore the original values 'cos those
%% have already been captured at a higher level
snip_ref(List) ->
    {Id, Ref} = get_id(List, []),
    Ref2 = trim(Ref),
    Typed = type_ref(Ref2),
    {Id, Typed}.

get_id([{{inline, close}, _}, {{punc, colon}, _}, {{ws, _}, _} | T], Acc) ->
    {reverse(Acc), T};
get_id([H | T], Acc) -> get_id(T, [H | Acc]).

trim(String) -> trim_left(trim_right(String)).

trim_right(String) -> reverse(trim_left(reverse(String))).

trim_left([{{ws, _}, _} | T]) -> trim_left(T);
trim_left([[] | T])           -> trim_left(T);
trim_left(List)               -> List.

snip(List) -> List2 = reverse(List),
              case List2 of
                  [{{lf, _}, _} | T] -> lists:reverse(T);
                  _                        -> List
              end.

type_ref([{string, String}, {{punc, colon}, _},
          {string, [$/, $/ | _]} | _T] = S) ->
    Return = case string:to_upper(String) of
                 "HTTPS" -> {url, make_ref(S)};
                 "HTTP"  -> {url, make_ref(S)};
                 _Other  -> {img, flatten([{tags, "<img src=\""}, S
                            | {tags, "\">"}])}
             end,
    {url, Return};
type_ref(String) -> {img, flatten([{tags, "<img src=\""}, String
                            | {tags, "\">"}])}.

make_ref(List) -> make_ref1(List, []).

make_ref1([{{ws, _}, _} | T], Acc) ->
    "<a href=\"" ++ flatten(reverse(Acc)) ++ "\">" ++ make_title(T) ++ "</a>";
make_ref1([], Acc) ->
    URL = flatten(reverse(Acc)), "<a href=\""
        ++ URL ++ "\">" ++ URL ++ "</a>";
make_ref1([{_, Orig} | T], Acc) ->
    make_ref1(T, [Orig | Acc]).

% a bit lax :(
make_title([{{punc, doublequote}, _} | List]) -> make_title1(List, []);
make_title([{{punc, singlequote}, _} | List]) -> make_title1(List, []);
make_title(List)                              -> make_title1(List, []).

make_title1([], Acc)                              -> flatten(reverse(Acc));
make_title1([{{punc, doublequote}, _} | _T], Acc) -> flatten(reverse(Acc));
make_title1([{{punc, singlequote}, _} | _T], Acc) -> flatten(reverse(Acc));
make_title1([{_, Orig} | T], Acc)                 -> make_title1(T, [Orig | Acc]).
%% end of ref processing

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Build the Lexed Token List
%%% This is a two part lexer, first it chunks the input and then on the second
%%% pass it gathers it into lines and types the lines
%%%
%%% NOTE that there are two different styles of processing lines:
%%% * markdown transformed
%%% * block
%%% inside block processing the whole text is dumped and just url encoded
%%% and the original text is always maintained during the lexing/parsing
%%% so that it can be recreated if the context requires it...
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lex(String) -> l1(String, [], []).

%% this is the terminal head which ends the parsing...
l1([], [], A2)             -> flatten(reverse(A2));
l1([], A1, A2)             -> l1([], [], [l2(A1) | A2]);
%% these two heads capture opening and closing tags
l1([$<, $/|T], A1, A2)     -> {Tag, NewT} = closingdiv(T, []),
                              l1(NewT, [], [Tag, l2(A1) | A2]);
l1([$< | T], A1, A2)       -> {Tag, NewT} = openingdiv(T),
                              l1(NewT, [], [Tag , l2(A1) | A2]);
%% these clauses are the normal lexer clauses
l1([$= | T], A1, A2)       -> l1(T, [], [{{md, eq}, "="}, l2(A1) | A2]);
l1([$- | T], A1, A2)       -> l1(T, [], [{{md, dash}, "-"}, l2(A1) | A2]);
l1([$# | T], A1, A2)       -> l1(T, [], [{{md, atx}, "#"}, l2(A1) | A2]);
l1([$> | T], A1, A2)       -> l1(T, [], [{{md, gt}, ">"}, l2(A1) | A2]);
l1([$+ | T], A1, A2)       -> l1(T, [], [{{md, plus}, "+"}, l2(A1) | A2]);
l1([$* | T], A1, A2)       -> l1(T, [], [{{md, star}, "*"}, l2(A1) | A2]);
l1([$_ | T], A1, A2)       -> l1(T, [], [{{md, underscore}, "_"}, l2(A1) | A2]);
l1([$1 | T], A1, A2)       -> l1(T, [], [{num, "1"}, l2(A1) | A2]);
l1([$2 | T], A1, A2)       -> l1(T, [], [{num, "2"}, l2(A1) | A2]);
l1([$3 | T], A1, A2)       -> l1(T, [], [{num, "3"}, l2(A1) | A2]);
l1([$4 | T], A1, A2)       -> l1(T, [], [{num, "4"}, l2(A1) | A2]);
l1([$5 | T], A1, A2)       -> l1(T, [], [{num, "5"}, l2(A1) | A2]);
l1([$6 | T], A1, A2)       -> l1(T, [], [{num, "6"}, l2(A1) | A2]);
l1([$7 | T], A1, A2)       -> l1(T, [], [{num, "7"}, l2(A1) | A2]);
l1([$8 | T], A1, A2)       -> l1(T, [], [{num, "8"}, l2(A1) | A2]);
l1([$9 | T], A1, A2)       -> l1(T, [], [{num, "9"}, l2(A1) | A2]);
l1([$0 | T], A1, A2)       -> l1(T, [], [{num, "0"}, l2(A1) | A2]);
l1([$. | T], A1, A2)       -> l1(T, [], [{{punc, fullstop}, "."}, l2(A1) | A2]);
l1([$: | T], A1, A2)       -> l1(T, [], [{{punc, colon}, ":"}, l2(A1) | A2]);
l1([$' | T], A1, A2)       -> l1(T, [], [{{punc, singlequote}, "'"}, l2(A1) | A2]); %'
l1([$" | T], A1, A2)       -> l1(T, [], [{{punc, doublequote}, "\""}, l2(A1) | A2]); %"
l1([$` | T], A1, A2)       -> l1(T, [], [{{punc, backtick}, "`"}, l2(A1) | A2]); %"
l1([$! | T], A1, A2)       -> l1(T, [], [{{punc, bang}, "!"}, l2(A1) | A2]); %"
l1([$\\ | T], A1, A2)      -> l1(T, [], [{{punc, backslash}, "\\"}, l2(A1) | A2]); %"
l1([$[ | T], A1, A2)       -> l1(T, [], [{{inline, open}, "["}, l2(A1) | A2]);
l1([$] | T], A1, A2)       -> l1(T, [], [{{inline, close}, "]"}, l2(A1) | A2]);
l1([?SPACE | T], A1, A2)   -> l1(T, [], [{{ws, sp}, " "}, l2(A1) | A2]);
l1([?TAB | T], A1, A2)     -> l1(T, [], [{{ws, tab}, "\t"}, l2(A1) | A2]);
l1([?CR, ?LF | T], A1, A2) -> l1(T, [], [{{lf, crlf}, [?CR , ?LF]}, l2(A1) | A2]);
l1([?LF | T], A1, A2)      -> l1(T, [], [{{lf, lf}, [?LF]}, l2(A1) | A2]);
%% this final clause accumulates line fragments
l1([H|T], A1, A2)          -> l1(T, [H |A1] , A2).

l2([])   -> [];
l2(List) -> {string, flatten(reverse(List))}.

%% need to put in regexes for urls and e-mail addies
openingdiv(String) ->
    case get_url(String) of
        {{url, URL}, R1} -> {{url, URL}, R1};
        not_url          ->
            case get_email_addie(String) of
                {{email, EM}, R2} -> {{email, EM}, R2};
                not_email         -> openingdiv1(String, [])
            end
    end.
    
openingdiv1([$/,$>| T], Acc) -> Acc2 = flatten(reverse(Acc)),
                                Tag = string:to_upper(Acc2),
                                {{{{{tag, self_closing}, Tag}, "<"
                                   ++ Acc2 ++ ">"}, Acc2}, T};
openingdiv1([$>| T], Acc)    -> Acc2 = flatten(reverse(Acc)),
                                Tag = string:to_upper(Acc2),
                                {{{{tag, open}, Tag}, "<"
                                  ++ Acc2 ++ ">"}, T};
openingdiv1([H|T], Acc)      -> openingdiv1(T, [H | Acc]).

closingdiv([$>| T], Acc) -> Acc2 = flatten(reverse(Acc)),
                            Tag = string:to_upper(Acc2),
                            {{{{tag, close}, Tag}, "<"
                              ++ Acc2 ++ ">"}, T};
closingdiv([H|T], Acc)   -> closingdiv(T, [H | Acc]).

get_url(String) -> HTTP_regex = "^(H|h)(T|t)(T|t)(P|p)(S|s)*://",
                   case re:run(String, HTTP_regex) of
                       nomatch    -> not_url;
                       {match, _} -> get_url1(String, [])
                   end.

get_url1([], Acc)            -> URL = flatten(reverse(Acc)),
                                {{url, URL}, []};
% allow escaped kets
get_url1([$\\, $> | T], Acc) -> get_url1(T, [$>, $\\ | Acc]);
get_url1([$> | T], Acc)      -> URL = flatten(reverse(Acc)),
                                {{url, URL}, T};
get_url1([H | T], Acc)       -> get_url1(T, [H | Acc]).

get_email_addie(String) ->
    Snip_regex = ">",
    case re:run(String, Snip_regex) of
        nomatch                -> not_email;
        {match, [{N, _} | _T]} ->
            {Possible, [$> | T]} = lists:split(N, String),
            EMail_regex = "[a-z0-9!#$%&'*+/=?^_`{|}~-]+"
                ++ "(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*"
                ++ "@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+"
                ++ "(?:[a-zA-Z]{2}|com|org|net|gov|mil"
                ++ "|biz|info|mobi|name|aero|jobs|museum)",
            case re:run(Possible, EMail_regex) of
                nomatch    -> not_email;
                {match, _} -> {{email, Possible}, T}
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Internal functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_esc_str(List, Urls, Imgs) -> m_esc(List, Urls, Imgs, []).

m_esc([], _U, _I, A)              -> flatten(reverse(A));
m_esc([{tags, Tag} | T], U, I, A) -> m_esc(T, U, I, [Tag | A]);
m_esc([H | T], U, I, A)           -> m_esc(T, U, I, [make_str([H], U, I) | A]).

    
make_str(List, _Urls, _Imgs)    -> make_str1(List, []).
make_str1([], Acc)              -> Flat = flatten(reverse(Acc)),
                                   htmlchars(Flat);
make_str1([{_, Orig} | T], Acc) -> make_str1(T, [Orig | Acc]).


htmlchars(List) -> htmlchars(List, []).
 
htmlchars([], Acc) -> flatten(reverse(Acc));
%% line ends are pushed to a space..
htmlchars([?LF | T], Acc)            -> htmlchars(T, ["\n" | Acc]);
htmlchars([?CR, ?LF | T], Acc)       -> htmlchars(T, ["\n" | Acc]);

%% emphasis is a bit strange - must be preceeded by or followed by
%% white space to work and can also be escaped
htmlchars([$\\, $*, $* | T], A)    -> htmlchars(T, [$*, $* | A]);
htmlchars([?TAB, $*, $* | T], A)   -> {T2, NewA} = strong(T, $*),
                                      htmlchars(T2, [NewA, ?TAB | A]);
htmlchars([?SPACE, $*, $* | T], A) -> {T2, NewA} = strong(T, $*),
                                      htmlchars(T2, [NewA, ?SPACE | A]);
htmlchars([$*, $* | T], A)         -> htmlchars(T, [$*, $* | A]);
%% likewise for strong
htmlchars([$\\, $* | T], A)        -> htmlchars(T, [$* | A]);
htmlchars([?TAB, $* | T], A)       -> {T2, NewA} = emphasis(T, $*),
                                      htmlchars(T2, [NewA, ?TAB | A]);
htmlchars([?SPACE, $* | T], A)     -> {T2, NewA} = emphasis(T, $*),
                                      htmlchars(T2, [NewA, ?SPACE | A]);
htmlchars([$* | T], A)             -> htmlchars(T, [$* | A]);
%% and again for underscores
htmlchars([$\\, $_, $_ | T], A)    -> htmlchars(T, [$_, $_ | A]);
htmlchars([?TAB, $_, $_ | T], A)   -> {T2, NewA} = strong(T, $_),
                                      htmlchars(T2, [NewA, ?TAB | A]);
htmlchars([?SPACE, $_, $_ | T], A) -> {T2, NewA} = strong(T, $_),
                                      htmlchars(T2, [NewA, ?SPACE | A]);
htmlchars([$_, $_ | T], A)         -> htmlchars(T, [$_, $_ | A]);
%% likewise for strong
htmlchars([$\\, $_ | T], A)        -> htmlchars(T, [$_ | A]);
htmlchars([?TAB, $_ | T], A)       -> {T2, NewA} = emphasis(T, $_),
                                      htmlchars(T2, [NewA, ?TAB | A]);
htmlchars([?SPACE, $_ | T], A)     -> {T2, NewA} = emphasis(T, $_),
                                      htmlchars(T2, [NewA, ?SPACE | A]);
htmlchars([$_ | T], A)             -> htmlchars(T, [$_ | A]);

%% handle backtick escaping
htmlchars([$`, $` | T], A)         -> {T2, NewA} = dblcode(T),  
                                       htmlchars(T2, [NewA | A]);
htmlchars([$` | T], A)             -> {T2, NewA} = code(T),
                                       htmlchars(T2, [NewA, A]);
htmlchars([$& | T], A)             -> htmlchars(T, ["&amp;" | A]);
htmlchars([$< | T], A)             -> htmlchars(T, ["&lt;" | A]);
htmlchars([$> | T], A)             -> htmlchars(T, ["&gt;" | A]);
htmlchars([?NBSP | T], A)          -> htmlchars(T, ["&nbsp;" | A]);
htmlchars([H | T], A)              -> htmlchars(T, [H | A]).

emphasis(List, Delim) -> interpolate(List, Delim, "em", []).
strong(List, Delim)   -> interpolate2(List, Delim, "strong", []).
dblcode(List)         -> interpolate2(List, $`, "code", []).
code(List)            -> interpolate(List, $`, "code", []).

%% interpolate is for single delimiters...
interpolate([], _Delim, Tag,  Acc)        -> {[], "<" ++ Tag ++ ">"
                                                 ++ reverse(Acc) ++ "</" ++ Tag ++ ">"};
interpolate([Delim | T], Delim, Tag, Acc) -> {T,  "<" ++ Tag ++ ">"
                                              ++ reverse(Acc) ++ "</" ++ Tag ++ ">"};
interpolate([H | T], Delim, Tag,  Acc)    -> interpolate(T, Delim, Tag, [H | Acc]).

%% interpolate two is for double delimiters...
interpolate2([], _Delim, Tag,  Acc)               -> {[], "<" ++ Tag ++ ">"
                                                  ++ reverse(Acc) ++ "</" ++ Tag ++ ">"};
interpolate2([Delim, Delim | T], Delim, Tag, Acc) -> {T,  "<" ++ Tag ++ ">"
                                                  ++ reverse(Acc) ++ "</" ++ Tag ++ ">"};
interpolate2([H | T], Delim, Tag,  Acc)           -> interpolate2(T, Delim, Tag, [H | Acc]).

%%%-------------------------------------------------------------------
%%%
%%% Unit Tests
%%%
%%%-------------------------------------------------------------------

unit_test_() ->
    [
     % Quick start
     ?_assert(conv("3 > 4")            == "<p>3 &gt; 4</p>"),
     ?_assert(conv("you *sad* bastard") == "<p>you <em>sad</em> bastard</p>"),
     ?_assert(conv("you **sad** bastard") == "<p>you <strong>sad</strong> bastard</p>"),
     ?_assert(conv("you _sad_ bastard") == "<p>you <em>sad</em> bastard</p>"),
     ?_assert(conv("you __sad__ bastard") == "<p>you <strong>sad</strong> bastard</p>"),
     ?_assert(conv("you*sad*bastard") == "<p>you*sad*bastard</p>"),
     ?_assert(conv("you_sad_bastard") == "<p>you_sad_bastard</p>"),
     ?_assert(conv("you \\*sad\\* bastard") == "<p>you *sad* bastard</p>"),
     ?_assert(conv("you \\_sad\\_ bastard") == "<p>you _sad_ bastard</p>"),
     % Breaking up in to lines
     ?_assert(conv("blah\nblah")       == "<p>blah\nblah</p>"),
     ?_assert(conv("blah\r\nblah")     == "<p>blah\nblah</p>"),
     ?_assert(conv("blah\r\nblah  \n") == "<p>blah\nblah</p><br />"),
     % Setext headers
     ?_assert(conv("blahblah\n====")   == "<h1>blahblah</h1>"),
     ?_assert(conv("blahblah\n-----")  == "<h2>blahblah</h2>"),
     % ATX headers
     ?_assert(conv("# blahblah")       == "<h1>blahblah</h1>"),
     ?_assert(conv("## blahblah")      == "<h2>blahblah</h2>"),
     ?_assert(conv("### blahblah")     == "<h3>blahblah</h3>"),
     ?_assert(conv("#### blahblah")    == "<h4>blahblah</h4>"),
     ?_assert(conv("##### blahblah")   == "<h5>blahblah</h5>"),
     ?_assert(conv("###### blahblah")  == "<h6>blahblah</h6>"),
     ?_assert(conv("####### blahblah") == "<h6>blahblah</h6>"),
     ?_assert(conv("# blahblah ###")   == "<h1>blahblah</h1>"),
     % Basic blockquotes
     ?_assert(conv("> blah")           == "<p>&gt; blah</p>"),
     ?_assert(conv("bleh\n> blah")     == "<p>bleh</p><blockquote><p>blah</p></blockquote>"),
     ?_assert(conv("bleh  \n> blah")   == "<p>bleh</p><br /><blockquote><p>blah</p></blockquote>"),
     ?_assert(conv("bleh  \n> > blah") == "<p>bleh</p><br /><blockquote><p>&gt; blah</p></blockquote>"),
     % Basic unordered lists
     ?_assert(conv("+ blah")           == "<ul><li>blah</li></ul>"),
     ?_assert(conv("+blah")            == "<p>+blah</p>"),
     ?_assert(conv("* blah")           == "<ul><li>blah</li></ul>"),
     ?_assert(conv("*blah")            == "<p>*blah</p>"),
     ?_assert(conv("- blah")           == "<ul><li>blah</li></ul>"),
     ?_assert(conv("-blah")            == "<p>-blah</p>"),
     ?_assert(conv("- a\n+ b\n- c")    == "<ul><li>a\n</li><li>b\n</li><li>c</li></ul>"),   
     ?_assert(conv("- a\n\n+ b")       == "<ul><li><p>a\n</p></li><li><p>b</p></li></ul>"),    
     ?_assert(conv("- a\n\n+ b\n\n+ c\n* d") == "<ul><li><p>a\n</p></li><li>" ++
              "<p>b\n</p></li><li><p>c\n</p></li><li>d</li></ul>"),   
     ?_assert(conv("- blah\nblah")     == "<ul><li>blah\nblah</li></ul>"),
     % Ordered Lists
     ?_assert(conv("1. blah")          == "<ol><li>blah</li></ol>"),
     ?_assert(conv("4. blah")          == "<ol><li>blah</li></ol>"),
     ?_assert(conv("555. blah")        == "<ol><li>blah</li></ol>"),
     ?_assert(conv("555" ++ [92, 46] ++ " blah") == "<p>555. blah</p>"),
     ?_assert(conv("555.blah")         == "<p>555.blah</p>"),
     ?_assert(conv("4. blah\nblah")    == "<ol><li>blah\nblah</li></ol>"),
     ?_assert(conv("4. a\n5. b\n6. c") == "<ol><li>a\n</li><li>b\n</li><li>c</li></ol>"),
     ?_assert(conv("4. a\n\n5. b\n\n6. c") == "<ol><li><p>a\n</p></li><li><p>b\n</p></li><li><p>c</p></li></ol>"),
     % Basic Code
     ?_assert(conv("    b")            == "<pre><code>b</code></pre>"),
     ?_assert(conv("\tb")              == "<pre><code>b</code></pre>"),
     % Code in listss
     ?_assert(conv("+ a\n\t    b")     == "<ul><li>a\n<br />b</li></ul>"),
     ?_assert(conv("+ a\n  \t  b")     == "<ul><li>a\n<br />b</li></ul>"),
     ?_assert(conv("+ a\n\t\tb")       == "<ul><li>a\n<br />b</li></ul>"),
     ?_assert(conv("+ a\n    > b")     == "<ul><li>a\n<blockquote>b</blockquote></li></ul>"),
     ?_assert(conv("+ a\n\t> b\nc")    == "<ul><li>a\n<blockquote>b\nc</blockquote></li></ul>"),
     ?_assert(conv("+ a\n\t> b\nc\n\nd")   == "<ul><li>a\n<blockquote>b\nc\n</blockquote>d</li></ul>"),
     ?_assert(conv("+ a\n\t> b\nc\n\n\nd") == "<ul><li>a\n<blockquote>b\nc\n</blockquote></li></ul><br /><p>d</p>"),
     ?_assert(conv("\t<div>")          == "<pre><code>&lt;div&gt;</code></pre>"),
     ?_assert(conv("\t<div>&")         == "<pre><code>&lt;div&gt;&amp;</code></pre>"),
     ?_assert(conv("+     blah<div>blah") == "<ul><li>    blah&lt;div&gt;blah</li></ul>"),
     ?_assert(conv("+        blah<div>blah") == "<ul><li><pre><code>blah&lt;div&gt;blah</code></pre></li></ul>"),
     ?_assert(conv("-        blah<div>blah") == "<ul><li><pre><code>blah&lt;div&gt;blah</code></pre></li></ul>"),
     ?_assert(conv("*        blah<div>blah") == "<ul><li><pre><code>blah&lt;div&gt;blah</code></pre></li></ul>"),
     % Block elements
     ?_assert(conv("\n\n<div>\n\n<table>\n\n</table>\n\n") == "<br /><br /><DIV><TABLE></TABLE>"),
     ?_assert(conv("blah<div>blah") == "<p>blah&lt;div&gt;blah</p>"),
     % Horizontal Rules
     ?_assert(conv("***") == "<hr />"),
     ?_assert(conv("---") == "<hr />"),
     ?_assert(conv("___") == "<hr />"),
     ?_assert(conv("*****") == "<hr />"),
     ?_assert(conv("-----") == "<hr />"),
     ?_assert(conv("_____") == "<hr />"),
     ?_assert(conv("* * *") == "<hr />"),
     ?_assert(conv("- - -") == "<hr />"),
     ?_assert(conv("_ _ _") == "<hr />"),
     ?_assert(conv("***blah") == "<p>***blah</p>"),
     ?_assert(conv("---blah") == "<p>---blah</p>"),
     ?_assert(conv("___blah") == "<p>___blah</p>")
    ].
