# Get path to main from commandline arguments
$main_exe_path = $args[0]


# Directory containing the tests
$test_dir = "$PSScriptRoot/tests"

# Count the number of tests
$test_count = (Get-ChildItem -Path $test_dir -Directory).Count

Write-Output "Running $test_count tests in $test_dir"

$passed_tests = 0
$failed_tests = 0

# Iterate over each subdirectory
Get-ChildItem -Path $test_dir -Directory | ForEach-Object {
  # Identify the source file and the text file
  $source_file = Get-ChildItem -Path $_.FullName -Filter "*.yay"
  $expected_output_file = Get-ChildItem -Path $_.FullName -Filter "*.txt"

  # Run the OCaml program on the source file
  & $main_exe_path $source_file.FullName | Out-File "$test_dir/temp_output.txt"

  # Compare the output with the expected output
  $diff = Compare-Object (Get-Content $test_dir/temp_output.txt) (Get-Content $expected_output_file.FullName)
  if ($diff) {
    Write-Output "Test in $($_.FullName) failed"
    Write-Output "Expected output:"
    Get-Content $expected_output_file.FullName | Write-Output
    Write-Output "Actual output:"
    Get-Content $test_dir/temp_output.txt | Write-Output
    $failed_tests += 1
  } else {
    Write-Output "$($_) passed"
    $passed_tests += 1
  }
}

# Clean up the temporary file
if (Test-Path "$test_dir/temp_output.txt") {
  Remove-Item "$test_dir/temp_output.txt"
}

# Print the number of tests that passed and failed
Write-Output "Number of tests passed: $passed_tests"
Write-Output "Number of tests failed: $failed_tests"