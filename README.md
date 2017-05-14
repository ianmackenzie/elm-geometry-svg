## What is it?

This [Elm](http://elm-lang.org) package provides functions to create and
manipulate SVG elements using the [OpenSolid](http://package.elm-lang.org/packages/opensolid/geometry/latest)
geometric data types. You can:

  - Draw OpenSolid 2D geometric objects as SVG
  - Apply OpenSolid-based 2D transformations to arbitrary SVG elements
  - Convert SVG between different coordinate systems

## Drawing

The `lineSegment2d`, `triangle2d`, `polyline2d`, `polygon2d`, and `circle2d`
functions all produce standard
[Svg msg](http://package.elm-lang.org/packages/elm-lang/svg/latest/Svg#Svg)
values that can be included in any SVG diagram:

![lineSegment2d](https://opensolid.github.io/images/svg/1.0/lineSegment2d.svg)
![triangle2d](https://opensolid.github.io/images/svg/1.0/triangle2d.svg)
![polyline2d](https://opensolid.github.io/images/svg/1.0/polyline2d.svg)
![polygon2d](https://opensolid.github.io/images/svg/1.0/polygon2d.svg)
![circle2d](https://opensolid.github.io/images/svg/1.0/circle2d.svg)

The appearance of the resulting elements can be customized by adding SVG
attributes such as `fill` and `stroke`.

## Transformation

The `scaleAbout`, `rotateAround`, `translateBy` and `mirrorAcross` functions
behave just like their standard OpenSolid counterparts. You can use them to do
things that would be difficult to do using just SVG, such as mirror a fragment
of SVG across an arbitrary axis:

![scaleAbout](https://opensolid.github.io/images/svg/1.0.2/scaleAbout.svg)
![rotateAround](https://opensolid.github.io/images/svg/1.0.2/rotateAround.svg)
![translateBy](https://opensolid.github.io/images/svg/1.0/translateBy.svg)
![mirrorAcross](https://opensolid.github.io/images/svg/1.0/mirrorAcross.svg)

## Coordinate conversion

The `relativeTo` and `placeIn` functions allow you to take SVG defined in one
coordinate system and convert it to another. For example, you can take SVG
defined in a model coordinate system where (0,0) is the center and positive Y is
up, and use `relativeTo` to convert it into SVG in window coordinates for
display, where (0,0) is the top left corner and positive Y is down.

`placeIn` is useful for 'instancing' or 'stamping' a fragment of SVG in many
different positions with different orientations:

![placeIn](https://opensolid.github.io/images/svg/1.0/placeIn.svg)

## Installation

Assuming you have [installed Elm](https://guide.elm-lang.org/install.html) and
started a new project, use [elm-package](https://guide.elm-lang.org/install.html#elm-package)
to install `opensolid/svg`, either by running

```
elm package install opensolid/svg
```

in a command prompt inside your project directory or by adding

```json
"opensolid/svg": "1.0.0 <= v < 2.0.0"
```

to the `dependencies` field in your project's `elm-package.json`.

## Documentation

[Full API documentation](http://package.elm-lang.org/packages/opensolid/svg/1.1.0/OpenSolid-Svg)
is available.

## Questions? Comments?

Please [open a new issue](https://github.com/opensolid/svg/issues) if you run
into a bug, if any documentation is missing/incorrect/confusing, or if there's a
new feature that you would find useful. For general questions about using this
package, try posting on:

  - [Elm Slack](http://elmlang.herokuapp.com/) (mention @ianmackenzie in your
    questions so I get a notification)
  - [Stack Overflow](https://stackoverflow.com/questions/ask?tags=opensolid+elm)
    (tag your question with 'opensolid' and 'elm')
  - The [r/elm](https://reddit.com/r/elm) subreddit
  - The [elm-discuss](https://groups.google.com/forum/#!forum/elm-discuss)
    Google Group
  - Or if you happen to be in the New York area, come on out to the
    [Elm NYC meetup](https://www.meetup.com/Elm-NYC/) =)

Have fun, and don't be afraid to ask for help!
