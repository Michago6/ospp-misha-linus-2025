#include <stdio.h>    // printf(), perror()
#include <stdlib.h>   // exit(), EXIT_SUCCESS, EXIT_FAILURE
#include <unistd.h>   // fork(), pipe(), dup2(), execlp()
#include <sys/wait.h> // waitpid()

#define READ  0
#define WRITE 1

void child_a(int fd[]) {
    close(fd[READ]);
    dup2(fd[WRITE], STDOUT_FILENO);
    execlp("ls", "ls", "-F", "-1", NULL);
    close(fd[WRITE]);
    exit(EXIT_SUCCESS);
}

void child_b(int fd[]) {
    close(fd[WRITE]);
    dup2(fd[READ], STDIN_FILENO);
    execlp("nl", "nl", NULL);
    close(fd[READ]);
    exit(EXIT_SUCCESS);
}

int main(void) {
    int fd[2];
    int status1, status2;
    pipe(fd) == -1 ? perror("pipe") : 0;

    pid_t id1 = fork();
    if (id1 == -1) {
        perror("fork");
        exit(EXIT_FAILURE);
    } else if (id1 == 0) {
        child_a(fd);
    } 

    pid_t id2 = fork();
    if (id2 == -1) {
        perror("fork");
        exit(EXIT_FAILURE);
    } else if (id2 == 0) {
        child_b(fd);
    } 
    close(fd[WRITE]);
    close(fd[READ]);
    int status;
    wait(&status);
    if(status == -1) {
        perror("wait");
    }
    wait(&status);
    if(status == -1) {
        perror("wait");
    }
    
    return EXIT_SUCCESS;
}
