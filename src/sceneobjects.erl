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

%% Scene Object Functions for rterl, a basic raytracer implementation in Erlang

-module(sceneobjects).
-export([normal/2]).
-export([intersect/2]).
-export([extract_material/1]).
-export([multiply_color/2]).
-export([add_colors/2]).

% Sphere: {sphere, {Radius, Center, Material}}
% Plane: {plane, {Normal, Offset, Material}}
% Intersection: {intersection, {SceneObject, Point, Distance}}
% Ray: {ray, {RayStart, RayDir}}
% Material: {material, {Color, Diffuse, Specularity, Alpha, Reflectivity}}
% Color: {color, {R, G, B}}

% gets the normal of an object at a point
normal(_, {plane, {Normal, _, _}}) ->
   Normal;
normal(Pos, {sphere, {_, Center, _}}) ->
   vectors:normalize(vectors:subtract(Pos, Center)).

% returns an intersection tuple of an object at a point (if one exists, otherwise nothing)
intersect({plane, {Normal, Offset, _}} = Plane, {ray, {RayStart, RayDir}}) ->
   Denom = vectors:dot_product(Normal, RayDir),
   if
      Denom >= 0 -> nothing;
      true ->  Dist = (vectors:dot_product(Normal, RayStart) + Offset) / (-1 * Denom),
            Point = vectors:add(RayStart, vectors:multiply(RayDir, Dist)),
            {intersection, {Plane, Point, Dist}}
   end;
intersect({sphere, {Radius, Center, _}} = Sphere, {ray, {RayStart, RayDir}}) ->
   Eo = vectors:subtract(Center, RayStart),
   V = vectors:dot_product(Eo, RayDir),
   Disc = (Radius * Radius) - (vectors:dot_product(Eo, Eo) - (V * V)),
   Dist = 
         if 
            Disc =< 0 -> 0;
            true -> V - math:sqrt(Disc)
         end,
   if 
      Dist =< 0 -> nothing;
      true -> {intersection, {Sphere, vectors:add(RayStart, vectors:multiply(RayDir, Dist)), Dist}}
   end.

% gets the material property of an object tuple
extract_material({plane, {_, _, Material}}) ->
   Material;
extract_material({sphere, {_, _, Material}}) ->
   Material.

% clamps a value within specified bounds
clamp(Val, _, To) when Val > To -> To;
clamp(Val, From, _) when Val < From -> From;
clamp(Val, _, _) -> Val.

% clamps a color to 0, 255
clamp_color({color, {R, G, B}}) ->
   {color, { clamp(R, 0, 255),
            clamp(G, 0, 255),
            clamp(B, 0, 255)}}.

% multiplies a color by another color
multiply_color({color, {R, G, B}}, Multiple) ->
   clamp_color({color, {R * Multiple, G * Multiple, B * Multiple}}).

% adds two colors
add_colors({color, {R1, G1, B1}}, {color, {R2, G2, B2}}) ->
   clamp_color({color, {R1 + R2, G1 + G2, B1 + B2}}).

