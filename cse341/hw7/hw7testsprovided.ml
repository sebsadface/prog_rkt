(* CSE341, Programming Languages, Homework 7, Provided Tests *)
(* Will not compile until you implement preprocess_prog and eval_prog *)

(* These tests do NOT cover all the various cases, especially for intersection *)

(* Must implement preprocess_prog and Shift before running these tests *)

(* Note these tests compare floats for equality which is generally a bad idea but makes sense
   when tests expect a particular answer *)

(* Preprocess tests *)
let _ = if preprocess_prog (LineSegment (3.2,4.1,3.2,4.1)) = Point (3.2,4.1)
        then print_string "preprocess converts a LineSegment to a Point successfully\n"
        else print_string "preprocess does not convert a LineSegment to a Point succesfully\n"

let _ = if preprocess_prog (LineSegment(-3.2,-4.1,3.2,4.1)) = LineSegment(3.2,4.1,-3.2,-4.1)
        then print_string "preprocess flips an improper LineSegment successfully\n"
        else print_string "preprocess does not flip an improper LineSegment successfully\n"

(* eval_prog tests *)

let run_prog e = eval_prog (preprocess_prog e) []

let do_test description input output = 
  print_string (description 
                ^ (if run_prog input = output then " is" else " is NOT")
                ^ " working properly\n")

let _ = do_test "shifting a point"
                (Shift (3.0, 4.0, Point (4.0,4.0))) 
                (Point (7.0,8.0)) 

let _ = do_test "using a variable"
                (Let ("a", Point (4.0,4.0), Shift (3.0,4.0,Var "a")))  
                (Point(7.0,8.0) ) 

let _ = do_test "using a shadowing variable"
                (Let ("a", Point (1.0,1.0), Let ("a", Point (4.0,4.0), Shift (3.0,4.0,Var "a")))) 
                (Point (7.0,8.0)) 
        
(* a few intersection tests *)

let _ = do_test "intersection: vertical segments overlapping" 
                (Intersect (LineSegment (0.0,0.0,0.0,2.0), LineSegment (0.0,1.0,0.0,3.0))) 
                (LineSegment (0.0,2.0,0.0,1.0))
        
let _ = do_test "intersection: vertical segment containment"
                (Intersect (LineSegment (0.0,0.0,0.0,4.0), LineSegment (0.0,1.0,0.0,3.0)))
                (LineSegment (0.0,3.0,0.0,1.0))

let _ = do_test "intersection: vertical segments no intersection"
                (Intersect (LineSegment (0.0,0.0,0.0,4.0), LineSegment (0.0,10.0,0.0,13.0)))
                NoPoints

let _ = do_test "intersection: vertical segments just touching" 
                (Intersect (LineSegment (0.0,0.0,0.0,4.0), LineSegment (0.0,4.0,0.0,5.0)))
                (Point (0.0,4.0))

let _ = do_test "intersection: overlapping non-vertical segments"
                (Intersect (LineSegment (0.0,0.0,2.0,4.0), LineSegment (1.0,2.0,3.0,6.0)))
                (LineSegment (2.0,4.0,1.0,2.0))                

let _ = do_test "intersection: non-vertical segment containment"
                (Intersect (LineSegment (0.0,0.0,3.0,6.0), LineSegment (1.0,2.0,2.0,4.0)))
                (LineSegment (2.0,4.0,1.0,2.0))

let _ = do_test "intersection: non-vertical segment containment, reversed order"
                (Intersect (LineSegment (1.0,2.0,2.0,4.0), LineSegment (0.0,0.0,3.0,6.0)))
                (LineSegment (2.0,4.0,1.0,2.0))
