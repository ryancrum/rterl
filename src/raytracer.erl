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

%% rterl, a basic raytracer implementation in Erlang

-module(raytracer).
-export([intersections/3, intersections/2, get_point/5, test/0, test/2, render/7, floor/1]).

% gets all intersections of a ray with a list of objects
intersections(Ray, Objects) ->
    intersections(Ray, Objects, 0).
intersections(Ray,
                Objects,
                DistanceLimit) ->
    % get our intersections...
    ISects = [sceneobjects:intersect(O, Ray) || O <- Objects, 
                                                sceneobjects:intersect(O, Ray) /= nothing],
    % and get the intersections that have a positive distance 
    % (meaning in front of the camera)
    ISectsCulled = if
        DistanceLimit > 0 ->
            [{intersection, {Object, Position, Distance}} 
                || {intersection, {Object, Position, Distance}} <- ISects,
                    Distance < DistanceLimit];
        true -> ISects
    end,
    % and sort the intersections by distance so we get proper Z-ordering
    SortByDistance = fun({intersection, {_, _, D1}}, {intersection,{_, _, D2}}) -> (D1 < D2) end,
    lists:sort(SortByDistance, ISectsCulled).

% defaults the depth to 5
trace_ray(Ray, Objects, Lights) ->
    trace_ray(Ray, Objects, Lights, 5).

% recursively traces a ray
trace_ray(_, _, _, 0) ->
    {color, {0, 0, 0}}; %todo: background color?
trace_ray({ray, {_, RayDir}} = Ray,
            Objects,
            Lights,
            MaxDepth) ->
    ISects = intersections(Ray, Objects),
    if
        length(ISects) =< 0 ->
            trace_ray(Ray, Objects, Lights, 0);
        true ->
            [Intersection |_] = ISects,
            {intersection, {Object, Point, _}} = Intersection,
            {material, {_, _, _, _, Reflectivity}} = sceneobjects:extract_material(Object),
            BaseColor = shade(Ray, Objects, Intersection, Lights),
            if 
                Reflectivity =< 0 -> 
                    BaseColor;
                true ->
                    Normal = sceneobjects:normal(Point, Object),
                    ReflectedVector = vectors:normalize(
                        vectors:add(RayDir, 
                                    vectors:multiply(Normal, 2))),
                    ReflectStart = vectors:add(Point,
                                                vectors:multiply(Normal, 0.001)),
                    ReflectedRay = {ray, {ReflectStart, ReflectedVector}},
                    ReflectedColor = sceneobjects:multiply_color(
                                            trace_ray(ReflectedRay,
                                                        Objects,
                                                        Lights,
                                                        MaxDepth - 1),
                                            Reflectivity),
                    sceneobjects:add_colors(BaseColor, ReflectedColor)
            end
    end.

% returns the color for an intersected ray
shade(Ray,
        Objects,
        Intersection,
        Lights) ->
    shade({color, {0, 0, 0}}, Ray, Objects, Intersection, Lights).
    
shade(BaseColor,
        _,
        _,
        _,
        []) ->
        BaseColor;

shade(BaseColor,
        {ray, {_, RayDir}} = Ray,
        Objects,
        {intersection, {SceneObject, Point, _}} = Intersection,
        Lights) ->
    [Light |NextLights] = Lights,
    VecToLight = vectors:normalize(vectors:subtract(
                                        lights:get_pos(Light),
                                        Point
                                    )),
    RayToLight = {ray, {vectors:add(Point, vectors:multiply(VecToLight, 0.001)), VecToLight}},
    DistToLight = abs(vectors:magnitude(vectors:subtract(lights:get_pos(Light), Point))),
    LightIntersections = intersections(RayToLight, Objects, DistToLight),
    if
        length(LightIntersections) > 0 ->
            shade(BaseColor, Ray, Objects, Intersection, NextLights);
        true ->
            {material, {MatColor, Diffuse, Specularity, _, _}} = sceneobjects:extract_material(SceneObject),
            Normal = sceneobjects:normal(Point, SceneObject),
            LightVector = lights:get_vector(Light, Point),
            Sh = Diffuse * vectors:dot_product(LightVector, Normal),
            ReflectVector = vectors:normalize(vectors:subtract(RayDir, vectors:multiply(Normal, 2))),
            Spec = 
                if
                    Specularity == 0 -> 0;
                    true -> math:pow(vectors:dot_product(LightVector, ReflectVector), Specularity)
                end,
            Illumination =
                if
                    Sh < 0 -> 0;
                    true -> Sh
                end,
            Highlight = sceneobjects:multiply_color({color, {255, 255, 255}}, Spec),
            DiffuseColor = sceneobjects:multiply_color(MatColor(Point), Illumination),
            TrueColor = sceneobjects:add_colors(Highlight, DiffuseColor),
            sceneobjects:add_colors(TrueColor, 
                        shade(BaseColor, Ray, Objects, Intersection, NextLights))
    end.

% gets the point for the projection
get_point(X, Y, {camera, {_, Forward, _, Right, Up}}, Width, Height) ->
    vectors:normalize(vectors:add(
        vectors:add(Forward,
            vectors:multiply(Right, recenter_x(X, Width))),
        vectors:multiply(Up, recenter_y(Y, Height)))).

% gets the scaled X and Y values
recenter_x(X, Width) ->
    (X - (Width / 2)) / (2 * Width).

recenter_y(Y, Height) ->
    -(Y - (Height / 2)) / (2 * Height).

% this is the spawned process function... it sends the pixel value back to the parent
render(Pixel, Objects, Camera, Lights, Width, Height, Pid) ->
    X = Pixel rem (Width),
    Y = (Pixel - X) / (Width),
    Dr = get_point(X, Y, Camera, Width, Height),
    Ray = {ray, {camera:get_position(Camera), Dr}},
    Color = trace_ray(Ray, Objects, Lights),
    Pid ! {{x, X}, {y, Y}, Color}.

% this spawns the processes that render each pixel
render(Objects, Camera, Lights, Width, Height) ->
    Pixels = lists:seq(0, (Width * Height) - 1),
    RenderFun = fun(Pixel) ->
        spawn(raytracer, render, [Pixel, Objects, Camera, Lights, Width, Height, self()])
    %    X = Pixel rem (Width),
    %    Y = (Pixel - X) / (Width),
    %    Dr = get_point(X, Y, Camera, Width, Height),
    %    Ray = {ray, {camera:get_position(Camera), Dr}},
    %    Color = trace_ray(Ray, Objects, Lights),
    %    {{x, X}, {y, Y}, Color}
    end,
    PixelCount = length(Pixels),
    lists:map(RenderFun, Pixels),
    wait_for_pixels(PixelCount, []).

% this receives all of the pixels from the spawned processes and when the final
% one is received, it returns the list full of them.
wait_for_pixels(0, Pixels) ->
    Pixels;
wait_for_pixels(Counter, Pixels) ->
    receive 
            %{{x, X}, {y, Y}, Color} -> io:format("~p~p\n", [X, Y]), %io:format("~p~n", [Pixel]),
            Pixel -> wait_for_pixels(Counter - 1, [Pixel | Pixels])
    end.

% creates a solid color function
solid_color(Color) ->
    fun(_) -> Color end.

% floor for decimal numbers
floor(X) ->
    T = trunc(X),
    case X - T < 0 of
        true -> T - 1;
        false -> T
    end.

% this sets up a scene with 3 spheres and a plane and renders it.
test() ->
    test(400, 400).
test(Width, Height) ->
    Checkerboard = fun({vector, {X, _, Z}}) ->
        Rem2 = (floor(X) + floor(Z)) rem 2,
        if
           Rem2 /= 0 -> {color, {255, 255, 255}};
           true -> {color, {0, 0, 0}}
        end
    end,
    Sphere = {sphere, {2, {vector, {0, 0, 0}}, {material, {solid_color({color, {0, 0, 0}}), 1.0, 20, 1, 0.9}}}},
    Sphere2 = {sphere, {0.6, {vector, {-3, 0, 0}}, {material, {solid_color({color, {0, 255, 255}}), 1.0, 4, 1, 0.05}}}},
    Sphere3 = {sphere, {8, {vector, {0, 7, 20}}, {material, {solid_color({color, {0, 255, 0}}), 1.0, 4, 1, 0.2}}}},
    Plane = {plane, {{vector, {0, 1, 0}}, 3, {material, {Checkerboard, 1.0, 0, 1, 0.5}}}},
    Camera = camera:build_camera({vector, {-6, 5, -12}}, {vector, {0, 0, 4}}),
    Light = {point_light, {{vector, {-20, 20, -20}}, 1.0, 100, {color, {255, 255, 255}}}},
    Light2 = {point_light, {{vector, {20, 20, -20}}, 1.0, 100, {color, {255, 0, 0}}}},
    Pixels = render([Sphere, Sphere2, Plane, Sphere3], Camera, [Light, Light2], Width, Height),
    [io:format("~w~n", [P]) || P <- Pixels].
