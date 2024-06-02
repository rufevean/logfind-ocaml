
let file = ".flogs"

let get_first_arg arr =
    if Array.length arr < 2 then (
        Printf.printf "Usage: %s [file1] [file2] ...\n" arr.(0);
        exit 1
    )
    else arr.(0)

let remove_first_arg arr =
    let len = Array.length arr in
    let new_arr = Array.make (len - 1) "" in
    for i = 1 to len - 1 do
        new_arr.(i - 1) <- arr.(i)
    done;
    new_arr

let () =
    let arg_array = Array.of_list (List.tl (Array.to_list Sys.argv)) in
    if Array.length arg_array < 1 then (
        Printf.printf "Usage: %s [file1] [file2] ...\n" Sys.argv.(0);
        exit 1
    );
    let flag = get_first_arg arg_array in
    let arg_array = remove_first_arg arg_array in
    let ic = open_in file in
    try
        while true do
            let line = input_line ic in
            let inner_file = line in
            let ic_ic = open_in inner_file in
            let mand = ref false in
            if flag = "-m" then
                mand := true
            else if flag = "-o" then
                mand := false
            else (
                Printf.printf "Usage: %s [file1] [file2] ...\n" Sys.argv.(0);
                exit 1
            );

            try
                while true do
                    let inner_line = input_line ic_ic in
                    for i = 0 to Array.length arg_array - 1 do
                        if String.contains inner_line arg_array.(i).[0] then (
                            Printf.printf "found in File %d, Name: %s\n" (i + 1) inner_file;
                            if !mand then (
                                close_in ic_ic;
                                raise Exit
                            )
                        )
                    done;
                done
            with End_of_file ->
                close_in ic_ic
        done
    with
    | Exit ->
        close_in ic
    | End_of_file ->
        close_in ic
    | e ->
        close_in_noerr ic;
        raise e
