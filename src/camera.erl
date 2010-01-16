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

%% Camera functions
%% Part of rterl by Ryan Crum
%% a very basic reference implementation of a raytracer in erlang

-module(camera).
-export([build_camera/2]).
-export([get_position/1]).

% Camera: {camera, {Position, Forward, Down, Right, Up}}

% Creates a camera tuple "object"
build_camera(Position, Target) ->
   Forward = vectors:normalize(vectors:subtract(Target, Position)),
   Down = {vector, {0, -1, 0}},
   Right = vectors:multiply(vectors:normalize(vectors:cross(Forward, Down)), 1.5),
   Up = vectors:multiply(vectors:normalize(vectors:cross(Forward, Right)), 1.5),
   {camera, {Position, Forward, Down, Right, Up}}.

% gets the position of a camera tuple
get_position({camera, {Position, _, _, _, _}}) ->
   Position.
