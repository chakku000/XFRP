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
  let include_arduino = "#include <Arduino..h>" in
  let include_m5stack = "#include <M5Stack..h>" in
  let task_bits = List.init thread (fun i -> Printf.sprintf "#define TASK%d_BIT (1 << %d)" i i) |> Utils.concat_without_empty "\n" in
  let all_task_bits =
    "#define ALL_TASK_BIT (" ^
    (List.init thread (fun i -> Printf.sprintf "TASK%d_BIT" i) |> Utils.concat_without_empty " | ") ^
    ")" in
  let fork_thread = "#define fork(i) xTaskCreatePinnedToCore(loop_name(i),\"Task##i\",4096,NULL,1,NULL,0)" in
  let decl_barrier = "EventGroupHandle_t barrier;" in
  let init_barrier = "#define init_barrier(thread) barrier = xEventGroupCreate();" in
  let sync = "#define synchronization(i) xEventGroupSync(barrier,TASK ## i ## _BIT ,ALL_TASK_BIT,portMAX_DELAY);" in
  Utils.concat_without_empty "\n" [ begin_guard; include_arduino; include_m5stack;
                                      task_bits; all_task_bits;
                                      fork_thread;
                                      decl_barrier; init_barrier; sync;
                                      end_guard]

let generate_user_setting_file (thread : int) (ast : Syntax.ast) (program : Module.program) =
  let filename = "setting.h" in
  let definitions_of_inputnodes = 
    List.map
      (function
        | Single(i,t) -> Printf.sprintf "%s definition_of_%s(){\n\treturn/*TODO:Implementation is Required*/;\n}" (Type.of_string t) i
        | Array((i,t),_,_) -> Printf.sprintf "%s definition_of_%s(int self){\n\treturn /*TODO:Implementation is Required */;\n}" (Type.of_string t) i
      ) ast.in_nodes
        |> String.concat "\n\n"
  in
  let out_c = open_out filename in
  let s = Utils.concat_without_empty "\n\n" [loop_name_generator ; use_pthread thread; user_esp32 thread; definitions_of_inputnodes] in
  Printf.fprintf out_c "%s" s;
  close_out out_c
