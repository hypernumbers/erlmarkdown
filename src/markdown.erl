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
%%% * it chops the lexed strings into lines which it represents as a list of lists
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
conv(String) -> Lex = lex(String),
                io:format("Lex is ~p~n", [Lex]),
                UntypedLines = make_lines(Lex),
                io:format("UntypedLines are ~p~n", [UntypedLines]),
                TypedLines = type_lines(UntypedLines),
                io:format("TypedLines are ~p~n", [TypedLines]),
                parse(TypedLines).

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

parse({UrlRefs, ImgRefs, TypedLines}) -> io:format("in parse UrlRefs are ~p~n" ++
                                                   "ImgRefs are ~p~n" ++
                                                   "TypedLines are ~p~n",
                                                   [UrlRefs, ImgRefs, TypedLines]),
                                         p1(TypedLines, UrlRefs, ImgRefs, [], []).


%% goes through the lines
%% If it hits a 'stast' like 'blockquote' it throws a HTML fragment into the mix
%% (eg <blockquote>) and then the matching closing one onto the stack (eg </blockquote>)
%% when it hits a hard return it then throws the matching closing one back in...
p1([], _Urls, _Imgs, [], Acc) ->
    flatten(reverse(Acc)); 
p1([], _Urls, _Imgs, Stack, Acc) ->
    flatten(reverse([reverse(Stack) | Acc]));
%% These clauses handle state issues
%% hit a blank line - you close the last open state (if it exists) and thow a <br />
p1([{blank, _} | T], Urls, Imgs, [], Acc) ->
    p1(T, Urls, Imgs, [], ["<br />" | Acc]); 
p1([{blank, _} | T], Urls, Imgs, [Close | Open], Acc) ->
    p1(T, Urls, Imgs, Open, flatten(["<br />", Close | Acc]));
%% two consecutive normal lines should be concatenated..
p1([{normal, P1}, {normal, P2} | T], Urls, Imgs, Stack, Acc) ->
    p1([{normal, flatten([P1 | P2])} | T], Urls, Imgs, Stack, Acc);
%% setext h1 is a look behind
p1([{normal, P}, {setext_h1, _} | T], Urls, Imgs, Stack, Acc) ->
    p1(T, Urls, Imgs, Stack, ["<h1>" ++ make_str(snip(P)) ++ "</h1>" | Acc]); 
%% setext h2 might be a look behind
p1([{normal, P}, {h2_or_hr, _} | T], Urls, Imgs, Stack, Acc) ->
    p1(T, Urls, Imgs, Stack, ["<h2>" ++ make_str(snip(P)) ++ "</h2>" | Acc]); 
%% blockquote is look ahead push blockquote onto the state
p1([{blockquote, _P} | T], Urls, Imgs, Stack, Acc) ->
    p1(T, Urls, Imgs, ["</blockquote>" | Stack], ["<blockquote>" | Acc]);
%% but one is just normal...
p1([{normal, P} | T], Urls, Imgs, Stack, Acc) ->
    p1(T, Urls, Imgs, Stack, ["<p>" ++ make_str(snip(P)) ++ "</p>" | Acc]);
%% atx headings
p1([{{h1, P}, _} | T], Urls, Imgs, Stack, Acc) ->
    p1(T, Urls, Imgs, Stack, ["<h1>" ++ string:strip(P, right) ++ "</h1>" | Acc]); 
p1([{{h2, P}, _} | T], Urls, Imgs, Stack, Acc) ->
    p1(T, Urls, Imgs, Stack, ["<h2>" ++ string:strip(P) ++ "</h2>" | Acc]); 
p1([{{h3, P}, _} | T], Urls, Imgs, Stack, Acc) ->
    p1(T, Urls, Imgs, Stack, ["<h3>" ++ string:strip(P) ++ "</h3>" | Acc]); 
p1([{{h4, P}, _} | T], Urls, Imgs, Stack, Acc) ->
    p1(T, Urls, Imgs, Stack, ["<h4>" ++ string:strip(P) ++ "</h4>" | Acc]); 
p1([{{h5, P}, _} | T], Urls, Imgs, Stack, Acc) ->
    p1(T, Urls, Imgs, Stack, ["<h5>" ++ string:strip(P) ++ "</h5>" | Acc]); 
p1([{{h6, P}, _} | T], Urls, Imgs, Stack, Acc) ->
    p1(T, Urls, Imgs, Stack, ["<h6>" ++ string:strip(P) ++ "</h6>" | Acc]);
%% unordered lists
p1([{{ul, _P}, _} | _T] = List, Urls, Imgs, Stack, Acc) ->
    {Rest, NewAcc} = parse_ol(List, Urls, Imgs, []),
    p1(Rest, Urls, Imgs, Stack, [NewAcc | Acc]);
p1([{br, P} | T], Urls, Imgs, Stack, Acc) ->
    p1(T, Urls, Imgs, Stack, [P | Acc]).

%% this is a bit messy because of the way that hard lines are treated...
%% If your li's have a blank line between them the item gets wrapped in a para,
%% if not, they don't
parse_ol([], _Urls, _Imgs, Acc) ->
    {[], "<ul>" ++ reverse(Acc) ++ "</ul>"};
%% if the '<li>' is followed by a blank then wrap it, throw the blank back onto the tail
parse_ol([{{ul, P}, _}, {blank, _} = B | T], _Urls, _Imgs, Acc) ->
    parse_ol([B | T], _Urls, _Imgs, ["<li><p>" ++ P ++ "</p></li>" | Acc]);
%% this is the partner of the previous line...
parse_ol([{blank, _}, {{ul, P}, _} | T], _Urls, _Imgs, Acc) ->
    parse_ol(T, _Urls, _Imgs, ["<li><p>" ++ P ++ "</p></li>" | Acc]);
%% this is a plain old '<li>'
parse_ol([{{ul, P}, _} | T], _Urls, _Imgs, Acc) ->
    parse_ol(T, _Urls, _Imgs, ["<li>" ++ P ++ "</li>" | Acc]);
%% parse_ol(List, _Urls, _Imgs, Acc) ->
%%    {List, "<ul" ++ reverse(Acc) ++ "</ul>"};
parse_ol(List, _Urls, _Imgs, Acc) ->
    {List, "<ul" ++ reverse(Acc) ++ "</ul>"}.
    

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
     {{linefeed, _}, _} = LF,
     {{md, gt}, _} = BQ,
     {{ws, _}, _} | T], A1, A2)            -> ml1(T, [], [[BQ], [{br, "<br />"}], ml2(LF, A1) | A2]);
%% this is a normal blockquote
ml1([{{linefeed, _}, _} = LF,
     {{md, gt}, _} = BQ,
     {{ws, _}, _} | T], A1, A2)            -> ml1(T, [], [[BQ], ml2(LF, A1) | A2]);
%% this clause forces a <BR /> line...
ml1([{{ws, _}, _},
     {{ws, _}, _},
     {{linefeed, _}, _} = H | []], A1, A2) -> ml1([], [], [[{br, "<br />"}], ml2(H, A1) | A2]);
%% this clause generates a blockquote...
ml1([{{linefeed, _}, _} = H | T], A1, A2)  -> ml1(T, [], [ml2(H, A1) | A2]);
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
type_lines(Lines) -> {UrlRefs, ImgRefs, TypedLines} = t_l1(Lines, [], [], []),
                     {UrlRefs, ImgRefs, TypedLines}.

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
t_l1([[{{linefeed, _}, _}| []]  = H | T], A1, A2, A3) ->
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
    t_l1(T, A1, A2, [{type_star2(H), [H | T]} | A3]);
t_l1([[{{md, plus}, _}, {{ws, _}, _} | T1] = H | T], A1, A2, A3) ->
    t_l1(T, A1, A2, [{{ul, make_str(T1)}, [H | T]} | A3]);

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
t_l1([[{br, P}] | T], A1, A2, A3) -> t_l1(T, A1, A2, [{br, P} | A3]);

%% Final clause
t_l1([H | T], A1, A2, A3) -> t_l1(T, A1, A2, [{normal , H} | A3]).

%%
%% Loads of type rules...
%%
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
    io:format("List is ~p~n", [List]),
    case trim_right(List) of
        [{{md, star}, _}, {{ws, _}, _},
         {{md, star}, _}, {{ws, _}, _},
         {{md, star}, _}]                -> hr;
        _Other -> case List of
                      [{{md, star}, _}, {{ws, _}, _} | T] -> {ul, make_str(T)};
                      _Other2                             -> normal
                  end
    end.

type_ol(List) -> {type_ol1(List, []), List}.

%% this line terminates on an escaped fullstop after a number
type_ol1([{num, _}, {{punc, backslash}, _}, {{punc, fullstop}, _} | _T], _Acc) ->
    normal;
%% we accumulate the digits in case we need to escape a full stop in a normal line
type_ol1([{num, _} = H | T], Acc) -> type_ol1(T, [H | Acc]);
type_ol1([{{punc, fullstop}, _} | T], _Acc) -> {ol, string:strip(make_str(T), left)};
type_ol1(_List, _Acc)                         -> normal.

%% strip trailing #'s as they are decorative only...
type_atx(List) -> Stripped = reverse(strip_atx(reverse(List))),
                  io:format("List is ~p~n Stripped is ~p~n", [List, Stripped]),
                  {type_atx1(Stripped, 0), List}.

strip_atx([{{md, atx}, _} | T]) -> strip_atx(T);
strip_atx(List)                 -> List.

%% this is final clause terminal
%% can't get a <h3> style header bigger than 6...
type_atx1([{{md, atx}, _} | T], 6) -> type_atx1(T, 6);
type_atx1([{{md, atx}, _} | T], N) -> type_atx1(T, N + 1);
type_atx1([{{ws, _}, _} | T], N)   -> type_atx1(T, N);
type_atx1([H | T], N)              -> Header = list_to_atom("h" ++ integer_to_list(N)),
                                      {Header, make_str([H | T])}.
                 
type_setext_h1(List) -> type_s_h1_1(List, []).

type_s_h1_1([], Acc)                      -> {setext_h1, reverse(Acc)};
type_s_h1_1([[] | T], Acc)                -> type_s_h1_1(T, Acc);
type_s_h1_1([{{md, eq}, _} = H | T], Acc) -> type_s_h1_1(T, [H | Acc]);
type_s_h1_1(L, Acc)                       -> {normal, [flatten(reverse(Acc)) | L]}.

type_setext_h2(List) ->
    case type_s_h2_1(List) of
        h2_or_hr -> {h2_or_hr, List};
        not_h2   -> {type_s_h2_2(trim_right(List)), List} % be permissive with trailing whitespace
    end.                            

type_s_h2_1([])                    -> h2_or_hr;
type_s_h2_1([[] | T])              -> type_s_h2_1(T);
type_s_h2_1([{{md, dash}, _} | T]) -> type_s_h2_1(T);
type_s_h2_1(_L)                    -> not_h2.

type_s_h2_2([{{md, dash}, _}, {{ws,_}, _},
             {{md, dash}, _}, {{ws, _}, _},
             {{md, dash}, _}])                   -> hr;
type_s_h2_2([{{md, dash}, _}, {{ws, _}, _} | T]) -> {ul, make_str(T)};
type_s_h2_2(_List)                               -> normal.
 
type_ws(List) -> case type_ws1(List) of
                     blank         -> {blank, List};
                     try_codeblock -> {type_ws2(List), List}
                 end.

type_ws1([])                 -> blank;
type_ws1([[] | T])           -> type_ws1(T);
type_ws1([{{ws, _}, _} | T]) -> type_ws1(T);
type_ws1(_L)                 -> try_codeblock.

type_ws2([{{ws, tab}, _} | T]) -> {codeblock, T};
type_ws2([{{ws, sp}, _}, {{ws, sp}, _}, {{ws, sp}, _}, {{ws, sp}, _} | T] ) ->
    {codeblock, T};
type_ws2(_List) -> normal.

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
                  [{{linefeed, _}, _} | T] -> lists:reverse(T);
                  _                        -> List
              end.

type_ref([{string, String}, {{punc, colon}, _}, {string, [$/, $/ | _]} | _T] = S) ->
    Return = case string:to_upper(String) of
                 "HTTPS" -> {url, make_ref(S)};
                 "HTTP"  -> {url, make_ref(S)};
                 _Other  -> {img, "<img src=\"" ++ make_str(S) ++ "\">"}
             end,
    {url, Return};
type_ref(String) -> {img, "<img src=\"" + make_str(String) ++ "\">"}.

make_ref(List) -> make_ref1(List, []).

make_ref1([{{ws, _}, _} | T], Acc) -> "<a href=\"" ++ flatten(reverse(Acc))
                                          ++ "\">" ++ make_title(T) ++ "</a>";
make_ref1([], Acc)                 -> URL = flatten(reverse(Acc)),
                                          "<a href=\"" ++ URL ++ "\">"++ URL ++ "</a>";
make_ref1([{_, Orig} | T], Acc)    -> make_ref1(T, [Orig | Acc]).

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
                              l1(NewT, A1, [Tag | A2]);
l1([$< | T], A1, A2)       -> {Tag, NewT} = openingdiv(T),
                              l1(NewT, A1, [Tag | A2]);
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
l1([?CR, ?LF | T], A1, A2) -> l1(T, [], [{{linefeed, crlf}, [?CR , ?LF]}, l2(A1) | A2]);
l1([?LF | T], A1, A2)      -> l1(T, [], [{{linefeed, lf}, [?LF]}, l2(A1) | A2]);
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
                                {{{tag, self_closing}, Tag, Acc2}, T};
openingdiv1([$>| T], Acc)    -> Acc2 = flatten(reverse(Acc)),
                                Tag = string:to_upper(Acc2),
                                {{{tag, open}, Tag}, T};
openingdiv1([H|T], Acc)      -> openingdiv1(T, [H | Acc]).

closingdiv([$>| T], Acc) -> Acc2 = flatten(reverse(Acc)),
                            Tag = string:to_upper(Acc2),
                            {{{tag, close}, Tag}, T};
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
make_str(List) -> make_str1(List, []).

make_str1([], Acc)              -> Flat = flatten(reverse(Acc)),
                                   htmlchars(Flat);
make_str1([{_, Orig} | T], Acc) -> make_str1(T, [Orig | Acc]).

htmlchars(List) -> htmlchars(List, []).
 
htmlchars([], Acc) -> flatten(reverse(Acc));
%% line ends are pushed to a space..
htmlchars([?LF | T], Acc)      -> htmlchars(T, [" " | Acc]);
htmlchars([?CR, ?LF | T], Acc) -> htmlchars(T, [" " | Acc]);
%% escape stars and underscores
htmlchars([$\\, $* | T], Acc)  -> htmlchars(T, [$* | Acc]);
htmlchars([$\\, $_ | T], Acc)  -> htmlchars(T, [$_ | Acc]);
htmlchars([$*, $* | T], Acc)   -> {T2, NewAcc} = strong(T, $*),
                                  htmlchars(T2, [NewAcc | Acc]);
htmlchars([$* | T], Acc)       -> {T2, NewAcc} = emphasis(T, $*),
                                  htmlchars(T2, [NewAcc | Acc]);
htmlchars([$_, $_ | T], Acc)   -> {T2, NewAcc} = strong(T, $_),
                                  htmlchars(T2, [NewAcc | Acc]);
htmlchars([$_ | T], Acc)       -> {T2, NewAcc} = emphasis(T, $_),
                                  htmlchars(T2, [NewAcc | Acc]);
%% handle backtick escaping
htmlchars([$`, $` | T], Acc)   -> {T2, NewAcc} = dblcode(T),  
                                  htmlchars(T2, [NewAcc | Acc]);
htmlchars([$` | T], Acc)       -> {T2, NewAcc} = code(T),
                                  htmlchars(T2, [NewAcc, Acc]);
htmlchars([$< | T], Acc)       -> htmlchars(T, ["&lt;" | Acc]);
htmlchars([$> | T], Acc)       -> htmlchars(T, ["&gt;" | Acc]);
htmlchars([?NBSP | T], Acc)    -> htmlchars(T, ["&nbsp;" | Acc]);
htmlchars([H | T], Acc)        -> htmlchars(T, [H | Acc]).

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
     ?_assert(conv("3 > 4")            == "<p>3 &gt; 4</p>"),
     ?_assert(conv("blah\nblah")       == "<p>blah blah</p>"),
     ?_assert(conv("blah\r\nblah")     == "<p>blah blah</p>"),
     ?_assert(conv("blah\r\nblah  \n") == "<p>blah blah</p><br />"),
     ?_assert(conv("blahblah\n====")   == "<h1>blahblah</h1>"),
     ?_assert(conv("blahblah\n-----")  == "<h2>blahblah</h2>"),
     ?_assert(conv("# blahblah")       == "<h1>blahblah</h1>"),
     ?_assert(conv("## blahblah")      == "<h2>blahblah</h2>"),
     ?_assert(conv("### blahblah")     == "<h3>blahblah</h3>"),
     ?_assert(conv("#### blahblah")    == "<h4>blahblah</h4>"),
     ?_assert(conv("##### blahblah")   == "<h5>blahblah</h5>"),
     ?_assert(conv("###### blahblah")  == "<h6>blahblah</h6>"),
     ?_assert(conv("####### blahblah") == "<h6>blahblah</h6>"),
     ?_assert(conv("# blahblah ###")   == "<h1>blahblah</h1>"),
     ?_assert(conv("> blah")           == "<p>&gt; blah</p>"),
     ?_assert(conv("bleh\n> blah")     == "<p>bleh</p><blockquote><p>blah</p></blockquote>"),
     ?_assert(conv("bleh  \n> blah")   == "<p>bleh</p><br /><blockquote><p>blah</p></blockquote>"),
     ?_assert(conv("bleh  \n> > blah") == "<p>bleh</p><br /><blockquote><p>&gt; blah</p></blockquote>"),
     ?_assert(conv("+ blah")           == "<ul><li>blah</li></ul>"),
     ?_assert(conv("+blah")            == "<p>+blah</p>"),
     ?_assert(conv("* blah")           == "<ul><li>blah</li></ul>"),
     ?_assert(conv("*blah")            == "<p><em>blah</em></p>"),
     ?_assert(conv("- blah")           == "<ul><li>blah</li></ul>"),
     ?_assert(conv("-blah")            == "<p>-blah</p>"),
     ?_assert(conv("- a\n+ b\n- c")    == "<ul><li>a </li><li>b </li><li>c</li></ul>"),   
     ?_assert(conv("- a\n\n+ b")       == "<ul><li><p>a </p></li><li><p>b</p></li></ul>"),    
     ?_assert(conv("- a\n\n+ b\n\n+ c\n* d") == "<ul><li><p>a </p></li><li><p>b </p></li>" ++
              "<li><p>c </p></li><li>d</li></ul>"),   
     ?_assert(conv("- blah\nblah")           == "<ul><li>blah blah</li></ul>")
    ].
