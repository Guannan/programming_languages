

fun sum2 xs =
    let fun f (xs,acc) =
	    case xs of
		[] => acc
	      | i::xs' => f(xs',i+acc)
    in
	f(xs,0)
    end

val t_sum2 = sum2 ([1,2,3]);

