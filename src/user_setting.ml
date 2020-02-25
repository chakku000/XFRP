open Syntax
let loop_name_generator = "#define loop_name(i) loop##i"

let use_pthread (thread : int) : string = 
  let begin_guard = "#ifdef XFRP_ON_PTHREAD" in
  let end_guard = "#endif" in
  let include_pthread = "#include <pthread.h>" in
  let decl_thrad = Printf.sprintf "pthread_t th[%d];" thread in
  let fork_thread = "#define fork(i) pthread_create(th[i], NULL, loop_name(i), NULL)" in
  let decl_barrier = "pthread_barrier_t barrier;" in
  let init_barrier = "#define init_barrier(thread) pthread_barrier_init(&barrier,NULL,(thread))" in
  let sync = "#define synchronization(tid) pthread_barrier_wait(&barrier);" in
  Utils.concat_without_empty "\n" [ begin_guard; include_pthread;
                                      decl_thrad; fork_thread;
                                      decl_barrier; init_barrier; sync;
                                      end_guard]

let user_esp32 (thread : int) : string = 
  let begin_guard = "#ifdef XFRP_ON_ESP32" in
  let end_guard = "#endif" in
  let include_arduino = "#include <Arduino.h>" in
  let include_m5stack = "#include <M5Stack.h>" in
  let task_bits = List.init thread (fun i -> Printf.sprintf "#define TASK%d_BIT (1 << %d)" i i) |> Utils.concat_without_empty "\n" in
  let all_task_bits =
    "#define ALL_TASK_BIT (" ^
    (List.init thread (fun i -> Printf.sprintf "TASK%d_BIT" i) |> Utils.concat_without_empty " | ") ^
    ")" in
  let fork_thread = "#define fork(i) xTaskCreatePinnedToCore(loop_name(i),\"Task##i\",8192,NULL,1,NULL,0)" in
  let decl_barrier = "EventGroupHandle_t barrier;" in
  let init_barrier = "#define init_barrier(thread) barrier = xEventGroupCreate();" in
  let sync = "#define synchronization(i) xEventGroupSync(barrier,TASK ## i ## _BIT ,ALL_TASK_BIT,portMAX_DELAY);" in
  Utils.concat_without_empty "\n" [ begin_guard; include_arduino; include_m5stack;
                                      task_bits; all_task_bits;
                                      fork_thread;
                                      decl_barrier; init_barrier; sync;
                                      end_guard]

let generate_input_function (program : Module.program) = 
  let args = 
    List.map
    (fun node -> 
      let id = Hashtbl.find program.id_table node in
      let info = Hashtbl.find program.info_table id in
      if info.number = 1 then Printf.sprintf "%s* %s" (Type.of_string info.t) node
                         else Printf.sprintf "%s %s[]" (Type.of_string info.t) node)
    program.input
    |> Utils.concat_without_empty ","
  in
  let head = Printf.sprintf "void input(%s){"  args in
  head ^ "\n" ^ "}"

let generate_output_function (program : Module.program) = 
  let args = 
    List.map
    (fun node -> 
      let id = Hashtbl.find program.id_table node in
      let info = Hashtbl.find program.info_table id in
      if info.number = 1 then Printf.sprintf "%s %s" (Type.of_string info.t) node 
                         else Printf.sprintf "%s %s[]" (Type.of_string info.t) node)
    program.output
    |> Utils.concat_without_empty ","
  in
  (Printf.sprintf "void output(%s){" args) ^ "\n" ^ "}"


let generate_user_setting_file (thread : int) (ast : Syntax.ast) (program : Module.program) =
  let filename = "setting.h" in
  let out_c = open_out filename in
  let input = generate_input_function program in
  let output = generate_output_function program in
  let s = Utils.concat_without_empty "\n\n" [loop_name_generator ; use_pthread thread; user_esp32 thread; input; output;] in
  Printf.fprintf out_c "%s" s;
  close_out out_c
