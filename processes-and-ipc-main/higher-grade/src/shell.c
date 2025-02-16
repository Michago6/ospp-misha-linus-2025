#include "parser.h"    // cmd_t, position_t, parse_commands()

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <stdbool.h>
#include <fcntl.h>     //fcntl(), F_GETFL

#define READ  0
#define WRITE 1


/**
 * For simplicitiy we use a global array to store data of each command in a
 * command pipeline .
 */
cmd_t commands[MAX_COMMANDS];

/**
 *  Debug printout of the commands array.
 */
void print_commands(int n) {
  for (int i = 0; i < n; i++) {
    printf("==> commands[%d]\n", i);
    printf("  pos = %s\n", position_to_string(commands[i].pos));
    printf("  in  = %d\n", commands[i].in);
    printf("  out = %d\n", commands[i].out);

    print_argv(commands[i].argv);
  }

}

/**
 * Returns true if file descriptor fd is open. Otherwise returns false.
 */
int is_open(int fd) {
  return fcntl(fd, F_GETFL) != -1 || errno != EBADF;
}

void fork_error() {
  perror("fork() failed)");
  exit(EXIT_FAILURE);
}

/**
 *  Fork a proccess for command with index i in the command pipeline. If needed,
 *  create a new pipe and update the in and out members for the command..
 */
void fork_cmd(int i, int num_commands, int fd_array[][2]) {
  pid_t pid;

  switch (pid = fork()) {
    case -1:
      fork_error();
      break;
    case 0:
      // Child process

      // Om detta inte är det första kommandot, omdirigera stdin från den föregående pipen.
      if (i > 0) {
        // 'pipes[i-1][0]' is the read end of the previous pipe.
        if (dup2(fd_array[i-1][0], STDIN_FILENO) == -1) {
          perror("dup2 stdin");
          exit(EXIT_FAILURE);
        }
      }

      // If this is not the last command, redirect stdout to the next pipe.
      if (i < num_commands - 1) {
        // 'pipes[i][1]' is the write end of the next pipe.
        if (dup2(fd_array[i][1], STDOUT_FILENO) == -1) {
          perror("dup2 stdout");
          exit(EXIT_FAILURE);
        }
      }

      // Close all pipe file descriptors in the child since they are duplicated now.
      for (int j = 0; j < num_commands - 1; j++) {
        close(fd_array[j][READ]);
        close(fd_array[j][WRITE]);
      }

      // Execute the command.
      execvp(commands[i].argv[0], commands[i].argv);
      
      // If execvp() fails, print error and exit.
      fprintf(stderr, "shell: command not found: %s\n", commands[i].argv[0]);
      exit(EXIT_FAILURE);
      break;
    default:
      // Parent process: continue and maybe store the child pid if needed.
      break;
  }
}


/**
 *  Fork one child process for each command inm the command pipeline.
 */
void fork_commands(int n) {

  //En array av file descriptors, en för varje pipe (n - 1).
  int fd_array[n - 1][2];
  
  for (int i = 0; i < n - 1; i++) {
      if (pipe(fd_array[i]) == -1) {
          perror("pipe");
          exit(EXIT_FAILURE);
      }
  }

// Samma loop som tidigare men nu tar fork_cmd in en array av file descriptors 
// antalet command
  for (int i = 0; i < n; i++) {
    fork_cmd(i, n, fd_array);
  }

  // Stäng alla pipe file descriptors i föräldern.
  for (int i = 0; i < n - 1; i++) {
    close(fd_array[i][READ]);
    close(fd_array[i][WRITE]);
  }
}

/**
 *  Reads a command line from the user and stores the string in the provided
 *  buffer.
 */
void get_line(char* buffer, size_t size) {
  getline(&buffer, &size, stdin);
  buffer[strlen(buffer)-1] = '\0';
}

/**
 * Make the parents wait for all the child processes.
 */
void wait_for_all_cmds(int n) {
  // Ändringen här är ganska easy peasy.
  int status;
  for (int i = 0; i < n; i++) {
    wait(&status);
    if (status == -1) {
      fprintf(stderr, "Command failed\n");
      exit(EXIT_FAILURE);
    }
  }
}


int main() {
  int n;               // Number of commands in a command pipeline.
  size_t size = 128;   // Max size of a command line string.
  char line[size];     // Buffer for a command line string.

  


  while(true) {
    printf(" >>> ");

    get_line(line, size);
    
    // printf(" get down ");

    n = parse_commands(line, commands);

    // printf(" parse down ");

    fork_commands(n);

    // printf(" fork donw ");
    
    wait_for_all_cmds(n);
  }

  exit(EXIT_SUCCESS);
}
