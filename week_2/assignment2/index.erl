-module(index).
-export([get_file_contents/1,show_file_contents/1, index_file/1]).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)


% Get the contents of a text file into a list of lines.

% Each line has its trailing newline removed.

get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
lists:reverse(Rev).

% Auxiliary function for get_file_contents.

% Not exported.

get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

% Show the contents of a list of strings.

% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
 show_file_contents([]) ->
    ok.


% index file

% create index of all words in file and print index to output
index_file(File) ->
  Lines = get_file_contents(File),
  Index = create_index(format_lines(Lines)),
  print_index(Index).
  

% getting file lines

% format file lines for as index input (unique list of words for every line)
format_lines(Lines) ->
  lists:map(fun(Line) -> format_line(Line) end, Lines).

% get unique list of lowercase words in line
format_line(Line) ->
  unique_list(lowercase_words(remove_short(split_line(Line)))).

% split line to list of words
split_line(Line) ->
    string:tokens(Line, " \t\n.,:;!?").

% remove short and empty strings from list
remove_short(Words) ->
  lists:filter(fun(X) -> string:len(X) > 2 end, Words).

% return all words to as lowercase
lowercase_words(Words) ->
  lists:map(fun(X) -> string:to_lower(X) end, Words).

% make list of unique elements (remove duplicates)
unique_list(List) ->
  lists:usort(List).

% create index


% create file index (index is stored in orddict)
create_index(Lines) ->
    Index = index_lines(1, Lines, orddict:new()),
    create_line_ranges(Index).

% index lines
index_lines(_LineNumber, [], Index) ->
    Index;
index_lines(LineNumber, [Line|Lines], Index) ->
    NewIndex = index_line(LineNumber, Line, Index),
    index_lines(LineNumber+1, Lines, NewIndex).

% index single line (index every word in line)
index_line(_LineNumber, [], Index) ->
    Index;
index_line(LineNumber, [Word|Words], Index) ->
    case orddict:find(Word, Index) of
        {ok, Value} ->
            % word is already present in index, update index record
            UpdatedIndex = orddict:store(Word, [LineNumber|Value], Index),
            index_line(LineNumber, Words, UpdatedIndex);
        error ->
            % word is not present in index, create new index record
            UpdatedIndex = orddict:store(Word, [LineNumber], Index),
            index_line(LineNumber, Words, UpdatedIndex)
    end.


%
% create ranges of lines
%

% format list of lines to ranges of lines in index
create_line_ranges(Index) ->
    orddict:map(fun(_Key, Value) -> create_ranges(Value) end, Index).

% create ranges of lines from list of lines
create_ranges(LineNumbers) ->
  create_ranges(LineNumbers, []).

% no lines to add to ranges, return ranges
create_ranges([], Out) ->
  Out;
% no ranges was created yet, create first range
create_ranges([H|T],[]) ->
  create_ranges(T, [{H,H}]);
% H item is in last range, no need to add new range
create_ranges([H|T], [{RangeBegin,RangeEnd}|Others]) when H > RangeBegin andalso H =< RangeEnd ->
  create_ranges(T, [{RangeBegin,RangeEnd}|Others]);
% H item extend last range, update last range
create_ranges([H|T], [{RangeBegin,RangeEnd}|Others]) when H > RangeBegin andalso H == RangeEnd+1 ->
  create_ranges(T, [{RangeBegin,H}|Others]);
% create new range from H item
create_ranges([H|T], Others) ->
  create_ranges(T, [{H,H}|Others]).


%
% print index
%

% print index
print_index(Index) ->
    IndexList = orddict:to_list(Index),
    print_index_list(IndexList).

% print every items in index
print_index_list([]) ->
    ok;
print_index_list([H|T]) ->
    print_index_item(H),
    print_index_list(T).

% print single index item
print_index_item({Key, Value}) ->
    io:format("{ ~p, ", [Key]),
    io:format("["),
    print_list_items(0, Value),
    io:format("] }~n").

% print list
print_list_items(_, []) ->
  ok;
print_list_items(0, [{RangeBegin,RangeEnd}|T]) ->
  io:format("{~p,~p}", [RangeBegin,RangeEnd]),
  print_list_items(1,T);
print_list_items(N, [{RangeBegin,RangeEnd}|T]) ->
  io:format(",{~p,~p}", [RangeBegin,RangeEnd]),
  print_list_items(N+1,T).
