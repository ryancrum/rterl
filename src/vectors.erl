%% Copyright (c) 2007, Ryan Crum
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     %% Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     %% Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     %% The name of Ryan Crum may not be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY RYAN CRUM ``AS IS'' AND ANY
%% EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL RYAN CRUM BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%% Vector Functions for rterl, a basic raytracer implementation in Erlang

-module(vectors).
-export([dot_product/2]).
-export([magnitude/1]).
-export([multiply/2]).
-export([divide/2]).
-export([normalize/1]).
-export([add/2]).
-export([subtract/2]).
-export([cross/2]).

% gets dot product of two vectors
dot_product({vector, {X1, Y1, Z1}}, {vector, {X2, Y2, Z2}}) ->
   (X1 * X2) + (Y1 * Y2) + (Z1 * Z2).

% gets the magnitude of a vector
magnitude(Vec) ->
   math:sqrt(dot_product(Vec, Vec)).

% scales a vector
multiply({vector, {X, Y, Z}}, Multiple) ->
   {vector, {X * Multiple, Y * Multiple, Z * Multiple}}.

% scales a vector
divide({vector, {X, Y, Z}}, Dividend) ->
   {vector, {X / Dividend, Y / Dividend, Z / Dividend}};
divide(Vec, 0) ->
   Vec.

% normalizes a vector
normalize(Vec) ->
   divide(Vec, magnitude(Vec)).

% adds two vectors
add({vector, {X1, Y1, Z1}}, {vector, {X2, Y2, Z2}}) ->
   {vector, {X1 + X2, Y1 + Y2, Z1 + Z2}}.

% subtracts two vectors
subtract({vector, {X1, Y1, Z1}}, {vector, {X2, Y2, Z2}}) ->
   {vector, {X1 - X2, Y1 - Y2, Z1 - Z2}}.

% cross product of two vectors
cross({vector, {X1, Y1, Z1}}, {vector, {X2, Y2, Z2}}) ->
   {vector, {(Y1 * Z2) - (Z1 * Y2),
            (Z1 * X2) - (X1 * Z2),
            (X1 * Y2) - (Y1 * X2)}}.
