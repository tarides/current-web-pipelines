let test_jobs_failed expected job_r () =
  Alcotest.(check bool) "" expected (Current_web_pipelines.Jobs.failed job_r)

let () =
  let ok_res : 't Current_web_pipelines.State.job_result = Ok `Blah in
  let error_active_ready : 't Current_web_pipelines.State.job_result =
    Error (`Active `Ready)
  in
  let error_active_running : 't Current_web_pipelines.State.job_result =
    Error (`Active `Running)
  in
  let error_blocked : 't Current_web_pipelines.State.job_result =
    Error `Blocked
  in
  let error_skipped_failure : 't Current_web_pipelines.State.job_result =
    Error `Skipped_failure
  in
  let error_skipped : 't Current_web_pipelines.State.job_result =
    Error (`Skipped "Blah")
  in
  let error_cancelled : 't Current_web_pipelines.State.job_result =
    Error `Cancelled
  in
  let error_message : 't Current_web_pipelines.State.job_result =
    Error (`Msg "Info")
  in
  Alcotest.run "Foo"
    [
      ( "Jobs_failed",
        [
          Alcotest.test_case "Jobs with result OK have not failed" `Quick
            (test_jobs_failed false ok_res);
          Alcotest.test_case
            "Jobs with result Error `Active `Ready have not failed" `Quick
            (test_jobs_failed false error_active_ready);
          Alcotest.test_case
            "Jobs with result Error `Active `Running have not failed" `Quick
            (test_jobs_failed false error_active_running);
          Alcotest.test_case "Jobs with result Error `Blocked have not failed"
            `Quick
            (test_jobs_failed false error_blocked);
          Alcotest.test_case
            "Jobs with result Error `Skipped_failure have not failed" `Quick
            (test_jobs_failed false error_skipped_failure);
          Alcotest.test_case "Jobs with result Error `Skipped have not failed"
            `Quick
            (test_jobs_failed false error_skipped);
          Alcotest.test_case "Jobs with result Error `Cancelled have failed"
            `Quick
            (test_jobs_failed true error_cancelled);
          Alcotest.test_case "Jobs with result Error `Msg have failed" `Quick
            (test_jobs_failed true error_message);
        ] );
    ]
