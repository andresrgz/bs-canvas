type canvas;

[@bs.deriving jsConverter]
type canvasFormat = [ | [@bs.as "svg"] `SVG | [@bs.as "pdf"] `PDF];

[@bs.new] [@bs.module]
external createCanvas':
  (~width: int=?, ~height: int=?, ~type_: string=?, unit) => canvas =
  "canvas";
let createCanvas = (~width=?, ~height=?, ~type_=?, ()) =>
  switch (type_) {
  | Some(type_) =>
    createCanvas'(~width?, ~height?, ~type_=type_ |> canvasFormatToJs, ())
  | None => createCanvas'(~width?, ~height?, ())
  };

type pngStream;
type jpegStream;
type pdfStream;

[@bs.deriving abstract]
type jpegStreamOptions = {
  [@bs.optional]
  bufsize: int,
  [@bs.optional]
  quality: int,
  [@bs.optional]
  progressive: bool,
};

[@bs.send] external pngStream: canvas => pngStream = "";
[@bs.send]
external jpegStream:
  (canvas, ~options: jpegStreamOptions=?, unit) => jpegStream =
  "";
[@bs.send] external pdfStream: canvas => pdfStream = "";

/* TODO: Add bindings for all possible forms of toBuffer function */
type buffer;
[@bs.send] external toBuffer: canvas => buffer = "";

[@bs.deriving jsConverter]
type imageFormat = [
  | [@bs.as "image/png"] `PNG
  | [@bs.as "image/jpeg"] `JPEG
];

/* TODO: Add binding for async toDataURL */
[@bs.send]
external toDataURL':
  (canvas, ~type_: string=?, ~encoderOptions: float=?, unit) => string =
  "";
let toDataURL = (canvas, ~type_=?, ~encoderOptions=?, ()) =>
  switch (type_) {
  | Some(type_) =>
    canvas->toDataURL'(~type_=type_ |> imageFormatToJs, ~encoderOptions?, ())
  | None => canvas->toDataURL'(~encoderOptions?, ())
  };

[@bs.deriving jsConverter]
type renderingContext = [ | [@bs.as "2d"] `CanvasRenderingContext2D];

module Image = {
  type t;
};

module CanvasRenderingContext2D = {
  type t;
  /* TODO: Add methods for gradient type */
  type gradient;
  /* TODO: Add methods for pattern type */
  type pattern;

  module TextMetrics = {
    [@bs.deriving abstract]
    type t =
      pri {
        width: float,
        actualBoundingBoxLeft: float,
        actualBoundingRight: float,
        fontBoundingBoxAscent: float,
        fontBoundingBoxDescent: float,
        actualBoundingBoxAscent: float,
        actualBoundingBoxDescent: float,
        emHeightAscent: float,
        emHeightDescent: float,
        alphabeticBaseline: float,
        ideographicBaseline: float,
      };
  };

  [@bs.get] external fontGet: t => string = "font";
  [@bs.set] external fontSet: (t, string) => unit = "font";

  [@bs.get] external lineWidthGet: t => int = "lineWidth";
  [@bs.set] external lineWidthSet: (t, int) => unit = "lineWidth";

  [@bs.get] external lineDashOffsetGet: t => int = "lineDashOffset";
  [@bs.set] external lineDashOffsetSet: (t, int) => unit = "lineDashOffset";

  [@bs.send] external getLineDash': t => array(int) = "";
  let getLineDash = t => t->getLineDash' |> Array.to_list;
  [@bs.send] external setLineDash': (t, array(int)) => unit = "";
  let setLineDash = (t, segments) =>
    t->setLineDash'(segments |> Array.of_list);

  [@bs.deriving jsConverter]
  type lineCap = [ | `butt | `round | `square | `unknown];
  [@bs.get] external lineCapGet': t => string = "lineCap";
  let lineCapGet = t =>
    switch (t->lineCapGet' |> lineCapFromJs) {
    | Some(lineCap) => lineCap
    | None => `unknown
    };
  [@bs.set] external lineCapSet': (t, string) => unit = "lineCap";
  let lineCapSet = (t, lineCap) => t->lineCapSet'(lineCap |> lineCapToJs);

  [@bs.deriving jsConverter]
  type lineJoin = [ | `bevel | `round | `miter | `unknown];
  [@bs.get] external lineJoinGet': t => string = "lineJoin";
  let lineJoinGet = t =>
    switch (t->lineJoinGet' |> lineJoinFromJs) {
    | Some(lineJoin) => lineJoin
    | None => `unknown
    };
  [@bs.set] external lineJoinSet': (t, string) => unit = "lineJoin";
  let lineJoinSet = (t, lineJoin) =>
    t->lineJoinSet'(lineJoin |> lineJoinToJs);

  [@bs.deriving jsConverter]
  type textAlign = [
    | `left
    | `right
    | `center
    | `start
    | [@bs.as "end"] `end_
    | `unknown
  ];
  [@bs.get] external textAlignGet': t => string = "textAlign";
  let textAlignGet = t =>
    switch (t->textAlignGet' |> textAlignFromJs) {
    | Some(textAlign) => textAlign
    | None => `unknown
    };
  [@bs.set] external textAlignSet': (t, string) => unit = "textAlign";
  let textAlignSet = (t, textAlign) =>
    t->textAlignSet'(textAlign |> textAlignToJs);

  [@bs.deriving jsConverter]
  type textBaseLine = [ | `top | `middle | `alphabetic | `bottom | `unknown];
  [@bs.get] external textBaseLineGet': t => string = "textBaseLine";
  let textBaseLineGet = t =>
    switch (t->textBaseLineGet' |> textBaseLineFromJs) {
    | Some(textBaseLine) => textBaseLine
    | None => `unknown
    };
  [@bs.set] external textBaseLineSet': (t, string) => unit = "textBaseLine";
  let textBaseLineSet = (t, textBaseLine) =>
    t->textBaseLineSet'(textBaseLine |> textBaseLineToJs);

  [@bs.get] external fillStyleColorGet: t => string = "fillStyle";
  [@bs.set] external fillStyleColorSet: (t, string) => unit = "fillStyle";
  [@bs.get] external fillStyleGradientGet: t => gradient = "fillStyle";
  [@bs.set] external fillStyleGradientSet: (t, gradient) => unit = "fillStyle";
  [@bs.get] external fillStylePatternGet: t => pattern = "fillStyle";
  [@bs.set] external fillStylePatternSet: (t, pattern) => unit = "fillStyle";

  [@bs.get] external strokeStyleColorGet: t => string = "strokeStyle";
  [@bs.set] external strokeStyleColorSet: (t, string) => unit = "strokeStyle";
  [@bs.get] external strokeStyleGradientGet: t => gradient = "strokeStyle";
  [@bs.set]
  external strokeStyleGradientSet: (t, gradient) => unit = "strokeStyle";
  [@bs.get] external strokeStylePatternGet: t => pattern = "strokeStyle";
  [@bs.set]
  external strokeStylePatternSet: (t, pattern) => unit = "strokeStyle";

  [@bs.send]
  external createLinearGradient: (t, int, int, int, int) => gradient = "";
  [@bs.send]
  external createRadialGradient: (t, int, int, int, int, int, int) => gradient =
    "";

  [@bs.deriving jsConverter]
  type repetition = [
    | `repeat
    | [@bs.as "repeat-x"] `repeatX
    | [@bs.as "repeat-y"] `repeatY
    | [@bs.as "no-repeat"] `noRepeat
  ];
  [@bs.send]
  external createPatternFromImage': (t, Image.t, string) => pattern =
    "createPattern";
  let createPatternFromImage = (t, image, repetion) =>
    t->createPatternFromImage'(image, repetion |> repetitionToJs);
  [@bs.send]
  external createPatternFromCanvas': (t, canvas, string) => pattern =
    "createPattern";
  let createPatternFromImage = (t, canvas, repetion) =>
    t->createPatternFromCanvas'(canvas, repetion |> repetitionToJs);

  [@bs.send] external clearRect: (t, int, int, int, int) => unit = "";
  [@bs.send] external fillRect: (t, int, int, int, int) => unit = "";
  [@bs.send] external strokeRect: (t, int, int, int, int) => unit = "";
  [@bs.send]
  external fillText: (t, string, int, int, ~maxWidth: int=?, unit) => unit =
    "";
  [@bs.send]
  external strokeText: (t, string, int, int, ~maxWidth: int=?, unit) => unit =
    "";
  [@bs.send] external measureText: (t, string) => TextMetrics.t = "";
};

[@bs.send] external getContext': (canvas, string) => 'a = "";
let getContext = (canvas, contextType) =>
  canvas->getContext'(contextType |> renderingContextToJs);
let getCanvasRenderingContext2D = canvas: CanvasRenderingContext2D.t =>
  canvas->getContext(`CanvasRenderingContext2D);