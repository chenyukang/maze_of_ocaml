(* ocamlopt -I /Users/kang/.opam/system/lib/lablgl lablgl.cmxa
              lablglut.cmxa str.cmxa unix.cmxa maze.ml -o maze *)

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
let end_point = ref (!maze_height * !maze_width - 1)
let union_set = ref (create_universe (!maze_width * !maze_height))
let edges = ref (Array.make (!maze_width * !maze_height - 1) (0, 0))

let remove_edge idx =
  let arr = Array.make (Array.length !edges - 1) (0, 0) and
      now = ref 0 in
  Array.iteri (fun i e ->
               if i <> idx then
                 begin
                   Array.set arr !now e;
                   now := !now + 1;
                 end)
               !edges;
  edges := arr

let next_edge () =
  let idx = Random.int (Array.length !edges) in
  let res = Array.get !edges idx in
  match res with
    (p1, p2) -> Printf.printf "value: %d %d\n" p1 p2;
  remove_edge idx;
  res

let generate_step () =
  if is_connect !union_set !start_point !end_point == false then
    let (p1, p2) = next_edge() in
    union !union_set p1 p2;
    (p1, p2)
  else
    (-1, -1)

let generate_maze (height, width) =
  maze_height := height;
  maze_width := width;
  let edge_num = (height - 1) * width + (width - 1) * height in
  let node_num = width * height in
  union_set := create_universe node_num;
  edges := Array.make edge_num (0, 0);
  end_point := node_num - 1;
  let now = ref 0 in
  for idx = 0 to node_num - 1 do
    let row = idx / width  in
    let col = idx mod height in
    if row < height - 1 then
      begin
        Array.set !edges !now (idx, (row + 1) * width + col);
        now := !now + 1;
      end;
    if col < width - 1 then
      begin
        Array.set !edges !now (idx, idx + 1);
        now := !now + 1;
      end
  done;
  let p = ref 1 in
  while !p >= 0 do
    let (p1, p2) = generate_step() in
    if p1 < 0 then
      p := p1;
  done

let vertex (x, y) =
  GlDraw.vertex2 (float line_width *. (0.5 +. float x),
                  float line_width *. (0.5 +. float y))

let render () =
  GlClear.clear [ `color ];
  GlMat.load_identity ();
  List.iter GlDraw.vertex2 [-1., -1.; 0., 1.; 1., -1.];
  GlDraw.color (1., 1., 1.);
  GlDraw.begins `line_strip;
  vertex (1, 2);
  vertex (1, 10);
  GlDraw.ends();

  GlDraw.begins `line_strip;
  vertex (2, 2);
  vertex (2, 10);
  GlDraw.ends();

  GlDraw.begins `line_strip;
  vertex (3, 2);
  vertex (3, 10);
  GlDraw.ends();
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
  let c = Char.chr key in
  match c with
    'q' | 'Q' -> exit 0
    | _ -> ()

let _ =
  ignore( Glut.init Sys.argv );
  Glut.initDisplayMode ~double_buffer:true ();
  Glut.initWindowSize !width !height;
  ignore(Glut.createWindow ~title: "Ocaml Maze");
  generate_maze(3, 3);
  Glut.reshapeFunc reshape;
  Glut.displayFunc ~cb:render;
  Glut.idleFunc ~cb:(Some Glut.postRedisplay);
  Glut.keyboardFunc keyboardFunc;
  Glut.mainLoop()
