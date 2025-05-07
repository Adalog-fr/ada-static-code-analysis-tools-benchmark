import subprocess

# Call complexity command to compute complexity of the project
# @link https://github.com/thoughtbot/complexity
# This script is not used in the benchmark, it is only here for information purpose

# Define a function to call the complexity command with specific parameters
def call_complexity_command():
    # Define the command and arguments as a list of strings
    command = ['complexity', '--format', 'json', '--only', '.ads', '.adb']

    # Execute the command using subprocess.run, capturing the output and error
    result = subprocess.run(command, capture_output=True, text=True)

    # Check if the command was executed successfully
    if result.returncode == 0:
        # Print the standard output from the command
        print("Command output:", result.stdout)
    else:
        # Print the standard error if the command failed
        print("Error:", result.stderr)

# Call the function to execute the command
call_complexity_command()
