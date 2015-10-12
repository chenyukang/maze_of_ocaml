(* ocamlopt -I /Users/kang/.opam/system/lib/lablgl lablgl.cmxa lablglut.cmxa maze.ml -o maze *)

type node = { mutable parent : int; mutable rank : int }
type universe = node array
type edge = { p1: int; p2: int}

let create_universe size =
  Array.init size (fun i -> { parent = i; rank = 0 })

let rec find (s : universe) (e : int) : int =
  let n = s.(e) in
  if n.parent = e then e
  else
    (n.parent <- (find s n.parent);
     n.parent)

let is_connect (s : universe) (p1: int) (p2: int) : bool =
  find s p1 = find s p2

let union s e1 e2 =
  let r1 = find s e1
  and r2 = find s e2 in
  let n1 = s.(r1)
  and n2 = s.(r2) in
  if (compare r1 r2) <> 0 then
    if n1.rank < n2.rank then
      n1.parent <- e2
    else
      (n2.parent <- e1;
       if n1.rank = n2.rank then
         n1.rank <- n1.rank + 1)

let width = ref 800
let height = ref 600
let line_width = 8
let maze_width = ref 40
let maze_height = ref 40
let start_point = ref 0
let move = 1
let center = ref 30
let end_point = ref (!maze_height * !maze_width - 1)
let union_set = (create_universe (!maze_width * !maze_height))
let edges: (int * int) array ref = ref [||]

let remove_edge edges idx =
  let arr: (int * int) array ref = ref [||] in
  Array.iteri (fun i e ->
               if i <> idx then
                 arr := Array.append !arr [|e|];
    )
    edges;
  !arr

let next_edge edges =
  let idx = Random.int (Array.length edges) in
  let pair = Array.get edges idx in
  match pair with
    (p1, p2) -> Printf.printf "remove: %d %d\n" p1 p2;
  (pair, remove_edge edges idx)

let rec generate_step union_set edges =
  if is_connect union_set !start_point !end_point == false then (
    let (p1, p2), edges = next_edge edges in
    union union_set p1 p2;
    generate_step union_set edges
  ) else edges

let generate_maze (height, width) =
  maze_height := height;
  maze_width := width;
  let node_num = width * height in
  let union_set = create_universe node_num in
  let edges = ref [||] in 
  end_point := node_num - 1;
  for row = 0 to height - 1 do
    for col = 0 to width - 1 do
      let cur = row * width + col in 
      if row < height - 1 then
        edges := Array.append !edges [|(cur, (row + 1) * width + col)|];
      if col < width - 1 then
        edges := Array.append !edges [|(cur, cur + 1)|];
    done
  done;
  Printf.printf "size: %d" (Array.length !edges);
  generate_step union_set !edges

let vertex (x, y) =
  GlDraw.vertex2 (float line_width *. (0.5 +. float x),
                  float line_width *. (0.5 +. float y))

let render_maze ()=
  Array.iteri (fun _ (p1, p2) ->
      let x1, y1 = move * (p1 / !maze_width), move * (p1 mod !maze_width) in
      let x2, y2 = move * (p2 / !maze_width), move * (p2 mod !maze_width) in
      GlDraw.begins `line_strip;
      if x1 == x2 then (
        vertex (x1 + !center, (max y1 y2) + !center);
        vertex (x1 + move + !center, (max y1 y2) + !center);
      );
      if y1 == y2 then (
        vertex ((max x1 x2) + !center, y1 + !center);
        vertex ((max x1 x2) + !center, y1 + move + !center);

      );
      GlDraw.ends();
    ) !edges;;

let render () =
  GlClear.clear [`color];
  GlMat.load_identity ();

  GlDraw.color (1., 1., 1.);

  GlDraw.begins `line_strip;
  vertex (!center, !center);
  vertex (!maze_width * move + !center, !center);
  GlDraw.ends();

  GlDraw.begins `line_strip;
  vertex (!maze_width * move + !center, !center);
  vertex (!maze_width * move + !center, !maze_height * move + !center);
  GlDraw.ends();

  GlDraw.begins `line_strip;
  vertex (!maze_width * move + !center, !maze_height * move + !center);
  vertex (!center, !maze_height * move + !center);
  GlDraw.ends();

  GlDraw.begins `line_strip;
  vertex (!center, !maze_height * move + !center);
  vertex (!center, !center);
  GlDraw.ends();

  render_maze();
  Glut.swapBuffers ()

(* Setup a 2D orthogonal projection. *)
let set_projection () =
  GlMat.mode `projection;
  GlMat.load_identity ();
  GlMat.ortho ~x:(0., float !width) ~y:(0., float !height) ~z:(0., 1.);
  GlMat.mode `modelview;
  GlMat.load_identity ()

let reshape ~w ~h =
  width := w;
  height := h;
  GlDraw.viewport ~x:0 ~y:0 ~w ~h;
  set_projection ();
  Gl.flush ()

let keyboardFunc ~key ~x ~y =
  match Char.chr key  with
    'q' | 'Q' -> exit 0
  | _ -> ()

let _ =
  ignore( Glut.init Sys.argv );
  Glut.initDisplayMode ~double_buffer:true ();
  Glut.initWindowSize !width !height;
  ignore(Glut.createWindow ~title: "Ocaml Maze");
  edges := generate_maze(40, 40);
  Printf.printf "size: %d" (Array.length !edges);
  Glut.reshapeFunc reshape;
  Glut.displayFunc ~cb:render;
  Glut.idleFunc ~cb:(Some Glut.postRedisplay);
  Glut.keyboardFunc keyboardFunc;
  Glut.mainLoop()
