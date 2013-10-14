
fun len xs =
    case xs of
	[] => 0
      | _::xs' => 1 + len xs'

val t_len = len ([1,1,1,1,1,1])

