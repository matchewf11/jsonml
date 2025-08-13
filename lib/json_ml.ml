let get_num str i =
  let len = String.length str in
  if i >= len then Error "i out of bounds"
  else
    let num_len =
      match String.get str i with
      | '-' | '.' | '0' .. '9' ->
          let rec helper i =
            if i >= len then i
            else
              match String.get str i with
              | '.' | '0' .. '9' -> helper (i + 1)
              | _ -> i
          in
          helper (i + 1) - i
      | _ -> 0
    in
    if num_len = 0 then Error "no number found"
    else
      try Ok (float_of_string @@ String.sub str i num_len, num_len)
      with Failure _ -> Error "invalid number"
