%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Gordon Guthrie
%%% @docs      generates a big fuzz file which we can use to
%%%            try and force bugs out of markdown
%%%
%%% @end
%%% Created : 10 Sep 2009 by gordonguthrie@backawinner.gg
%%%-------------------------------------------------------------------

-module(fuzz).

-export([fuzzit/0,
         get_string/0]).

-define(SPACE, 32).
-define(TAB, 9).
-define(LF, 10).
-define(CR, 13).
-define(NBSP, 160).

-define(LENGTH, 10000).

%% these two heads capture opening and closing tags

fuzzit() ->
    String = get_string(),
    Markdown = markdown:conv(String),
    _Json = (mochijson:encoder([{input_encoding, utf8}]))(Markdown).

get_string() -> get_string1(?LENGTH, []).

get_string1(0, Acc) -> lists:flatten(Acc);
get_string1(N, Acc) ->
    Random = random:uniform(),
    Type = round(random:uniform() * 3),
    get_string1(N - 1, [get_string2(Type, Random) | Acc]).

get_string2(Type, N) ->
    case Type of
        0 -> get_num(N);
        1 -> get_alpha(N);
        2 -> get_struct(N);
        3 -> get_struct2(N)
    end.

get_num(N) ->
    N1 = round(N *9),
    case N1 of
        0 -> $0;
        1 -> $1;
        2 -> $2;
        3 -> $3;
        4 -> $4;
        5 -> $5;
        6 -> $6;
        7 -> $7;
        8 -> $8;
        9 -> $9
    end.                              

get_alpha(N) -> 64 + round(N * 26).

get_struct(N) ->
    N1 = round(N * 36),
    case N1 of
        0 -> ?NBSP;
        1 -> $<; %
        2 -> $/;
        3 -> $<;
        4 -> $=;
        5 -> $-;
        6 -> $#;
        7 -> $>;
        8 -> $+;
        9 -> $*;
        10 -> $_;
        11 -> $1;
        12 -> $2;
        13 -> $3;
        14 -> $4;
        15 -> $5;
        16 -> $6;
        17 -> $7;
        18 -> $8;
        19 -> $9;
        20 -> $0;
        21 -> $.;
        22 -> $:;
        23 -> $';
        24 -> $";
        25 -> $`;
        26 -> $!;
        27 -> $\\;
        28 -> $/;
        29 -> $(;
        30 -> $);
        31 -> $[;
        32 -> $];
        33 -> ?SPACE;
        34 -> ?TAB;
        35 -> ?CR;
        36 -> ?LF
end.

get_struct2(N) ->
    N1 = round(N * 12),
    case N1 of
        0 -> "    ";
        1 -> [?CR, ?LF, ?CR, ?LF];
        2 -> [?LF, ?LF];
        3 -> [?TAB, ?TAB];
        4 -> ?SPACE;
        _ -> get_struct(N) % I know just makes it more balanced
    end.
            
