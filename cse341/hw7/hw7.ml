(* CSE341, Programming Languages, Homework 7, hw7.ml (see also Racket code) *)

(* Do not make changes to this code except where you see comments containing
   the word CHANGE. *)

(* expressions in a little language for 2D geometry objects
   values: no-points, points, lines, vertical lines, line segments
   other expressions: intersection of two expressions, lets, variables, 
                      (shifts added by you)
*)
type geom_exp = 
  | NoPoints
  | Point of float * float (* represents point (x,y) *)
  | Line of float * float (* represents line (slope, intercept) *)
  | VerticalLine of float (* x value *)
  | LineSegment of float * float * float * float (* x1,y1 to x2,y2 *)
  | Intersect of geom_exp * geom_exp (* intersection expression *)
  | Let of string * geom_exp * geom_exp (* let s = e1 in e2 *)
  | Var of string
  | Shift of float * float * geom_exp
(* CHANGE add shifts for expressions of the form Shift(deltaX, deltaY, exp) *)

exception BadProgram of string
exception Impossible of string

(* helper functions for comparing floats since rounding mean we should not 
   compare for equality *)

let epsilon = 0.00001

let float_close f1 f2 = 
  (Float.abs (f1 -. f2)) < epsilon

let float_close_point x1 y1 x2 y2 = 
  float_close x1 x2 && float_close y1 y2

(* helper function to return the Line or VerticalLine containing 
   points (x1,y1) and (x2,y2). Actually used only when intersecting 
   line segments, but might be generally useful *)
let two_points_to_line (x1,y1,x2,y2) = 
  if float_close x1 x2
  then VerticalLine x1
  else let m = (y1 -. y2) /. (x1 -. x2) in
       let b = y2 -. m *. x2 in
       Line (m,b)

(* helper function for interpreter: return value that is the intersection
   of the arguments: 25 cases because there are 5 kinds of values, but
   many cases can be combined, especially because intersection is commutative.
   Do *not* call this function with non-values (e.g., shifts or lets)
 *)
let rec intersect v1 v2 =
  match (v1,v2) with

  | (NoPoints, _) -> NoPoints (* 5 cases *)
  | (_, NoPoints) -> NoPoints (* 4 additional cases *)

  |	(Point (x,y), Point (x',y'))   -> if float_close_point x y x' y' then v1 else NoPoints
  | (Point (x,y), Line (m,b))      -> if float_close y (m *. x +. b) then v1 else NoPoints
  | (Point (x,_), VerticalLine x') -> if float_close x x'            then v1 else NoPoints
  | (Point _,     LineSegment _)   -> intersect v2 v1

  | (Line (m1,b1), VerticalLine x2) -> Point (x2, m1 *. x2 +. b1)
  | (Line (m1,b1), Line (m2,b2))    -> if float_close m1 m2 
                                       then (if float_close b1 b2
                                             then v1 (* same line *)
                                             else NoPoints) (* parallel lines do not intersect *)
                                       else (* one-point intersection *)
                                            let x = (b2 -. b1) /. (m1 -. m2) in
                                            let y = m1 *. x +. b1 in
                                            Point (x,y)
  | (Line _,       LineSegment _)   -> intersect v2 v1  
  | (Line _,       Point _)         -> intersect v2 v1
  
  | (VerticalLine x1, VerticalLine x2) -> if float_close x1 x2 then v1 else NoPoints
  | (VerticalLine _,  Point _)         -> intersect v2 v1
  | (VerticalLine _,  Line _)          -> intersect v2 v1
  | (VerticalLine _,  LineSegment _)   -> intersect v2 v1

  | (LineSegment (x1,y1,x2,y2), _) -> 
         (* the hard case, actually 4 cases because v2 could be a 
          point, line, vertical line, or line segment *)
       (* First compute the intersection of (1) the line containing the segment 
          and (2) v2. Then use that result to compute what we need. *)		
    (match intersect (two_points_to_line (x1,y1,x2,y2)) v2 with
     | NoPoints -> NoPoints 
     | Point(x0,y0) -> (* see if the point is within the segment bounds *)
       (* assumes v1 was properly preprocessed *)
       let inbetween v end1 end2 =
         (end1 -. epsilon <= v && v <= end2 +. epsilon)
         || (end2 -. epsilon <= v && v <= end1 +. epsilon) 
       in
       if inbetween x0 x2 x1 && inbetween y0 y2 y1
       then Point (x0,y0)
       else NoPoints
        
     | Line _ -> v1 (* so the segment is on line v2 *)
     | VerticalLine _ -> v1 (* so the segment is on vertical-line v2 *)
     | LineSegment (ox1,oy1,ox2,oy2) -> 
      (* the hard case in the hard case: the two segments are on the same
             line (or vertical line), but they could be (1) disjoint or 
             (2) overlapping or (3) one inside the other or (4) just touching.
         And we treat vertical segments differently, so there are 4*2 cases.
       *)
       let seg = (x1,y1,x2,y2) in
       let seg2 = (ox1,oy1,ox2,oy2) in
       if float_close x1 x2
       then (* the segments are on a vertical line *)
         (* let segment a start at or below start of segment b *)
         let ((aXend,aYend,aXstart,aYstart), 
              (bXend,bYend,bXstart,bYstart)) = if y2 < oy2 then (seg,seg2) else (seg2,seg)
         in if float_close aYend bYstart
            then Point (aXend,aYend) (* just touching *)
            else if aYend < bYstart
            then NoPoints (* disjoint *)
            else if aYend > bYend
            then LineSegment(bXend,bYend,bXstart,bYstart) (* b inside a *)
            else LineSegment(aXend,aYend,bXstart,bYstart) (* overlapping *)
       else (* the segments are on a non-vertical line *)
        (* let segment a start at or to the left of start of segment b *)
         let ((aXend,aYend,aXstart,aYstart),
              (bXend,bYend,bXstart,bYstart)) = if x2 < ox2 then (seg,seg2) else (seg2,seg)
         in if float_close aXend bXstart
            then Point (aXend,aYend) (* just touching *)
            else if aXend < bXstart
            then NoPoints (* disjoint *)
            else if aXend > bXend
            then LineSegment(bXend,bYend,bXstart,bYstart) (* b inside a *)
            else LineSegment(aXend,aYend,bXstart,bYstart) (* overlapping *)
                                
      | _ -> raise (Impossible "bad result from intersecting with a line")) 
  | _ -> raise (Impossible "bad call to intersect: only for shape values")

(* interpreter for our language: 
   * takes a geometry expression and returns a geometry value
   * for simplicity we have the top-level function take an environment,
     (which should be [] for the whole program
   * we assume the expression e has already been "preprocessed" as described
     in the homework assignment: 
         * line segments are not actually points (endpoints not float close)
         * lines segment have right (or, if vertical, top) coordinate first
*)

let rec eval_prog e env =
  match e with
  | NoPoints -> e (* first 5 cases are all values, so no computation *)
  | Point _  -> e
  | Line _   -> e
  | VerticalLine _ -> e
  | LineSegment _  -> e
  | Var s -> (match List.find_opt (fun (s2,_) -> s=s2) env with
               | None -> raise (BadProgram("var not found: " ^ s))
               | Some (_,v) -> v)
  | Let(s,e1,e2) -> eval_prog e2 ((s, eval_prog e1 env) :: env)
  | Intersect(e1,e2) -> intersect (eval_prog e1 env) (eval_prog e2 env)
  | Shift (dx,dy,exp) -> match exp with
                          | NoPoints -> exp
                          | Point (x,y) -> Point (x +. dx, y +. dy)
                          | Line (m,b) -> Line (m, b +. dy -. m *. dx)
                          | VerticalLine x -> VerticalLine (x +. dx)
                          | LineSegment (x1,y1,x2,y2) -> LineSegment (x1 +. dx, y1 +. dy, x2 +. dx, y2 +. dy)
                          | Intersect _ -> eval_prog (Shift (dx,dy,eval_prog exp env)) env
                          | Let _ -> raise (BadProgram "Let expression in Shift")
                          | Var s -> eval_prog (Shift (dx,dy,eval_prog exp env)) env
                          | Shift _ -> eval_prog (Shift (dx,dy,eval_prog exp env)) env
  (* CHANGE: Add a branch for Shift expressions *)

(* CHANGE: Add function preprocess_prog of type geom_exp -> geom_exp *)
let rec preprocess_prog e = 
  match e with 
  | NoPoints -> e
  | Point _ -> e
  | Line _ -> e
  | VerticalLine _ -> e
  | LineSegment (x1,y1,x2,y2) -> if float_close x1 x2 && float_close y1 y2
                                 then Point (x1,y1)
                                 else if (float_close x1 x2 && y2 > y1) || x2 > x1
                                 then LineSegment (x2,y2,x1,y1)
                                 else LineSegment (x1,y1,x2,y2)
  | Intersect(e1,e2) -> Intersect(preprocess_prog e1,  preprocess_prog e2)
  | Let(s,e1,e2) -> Let(s, preprocess_prog e1, preprocess_prog e2)
  | Var _ -> e
  | Shift(dx,dy,e) -> Shift(dx,dy,preprocess_prog e)