/* On Mac OS (aka OS X) the ucontext.h functions are deprecated and requires the
   following define.
*/
#define _XOPEN_SOURCE 700

/* On Mac OS when compiling with gcc (clang) the -Wno-deprecated-declarations
   flag must also be used to suppress compiler warnings.
*/

#include <signal.h> /* SIGSTKSZ (default stack size), MINDIGSTKSZ (minimal
                         stack size) */
#include <stdio.h>  /* puts(), printf(), fprintf(), perror(), setvbuf(), _IOLBF,
                         stdout, stderr */
#include <stdlib.h> /* exit(), EXIT_SUCCESS, EXIT_FAILURE, malloc(), free() */
#include <ucontext.h> /* ucontext_t, getcontext(), makecontext(),
                         setcontext(), swapcontext() */
#include <stdbool.h>  /* true, false */

#include "sthreads.h"

/* Stack size for each context. */
#define STACK_SIZE SIGSTKSZ * 100

/*******************************************************************************
                             Global data structures                              

                Add data structures to manage the threads here.
********************************************************************************/
thread_t *first_thread;
thread_t *latest_thread;
tid_t global_tid = 0;



/*******************************************************************************
                             Auxiliary functions

                      Add internal helper functions here.
********************************************************************************/
void helper() {

}

/*******************************************************************************
                    Implementation of the Simple Threads API
********************************************************************************/
int init() { 
    thread_t *thread = malloc(sizeof(thread_t));
    if (thread == NULL) return -1;
    thread->tid = global_tid;
    global_tid++;
    thread->state = running;
    thread->next = NULL;

    makecontext(&(thread->ctx), NULL, 0);

    latest_thread = thread;
    first_thread = thread;
    return 1;
}

tid_t spawn(void (*start)()) { 

    thread_t *thread = malloc(sizeof(thread_t));
    if (thread == NULL) return -1;
    thread->tid = global_tid;
    global_tid++;
    thread->state = running;
    thread->next = latest_thread;

    makecontext(&(thread->ctx), NULL, 0);

    latest_thread = thread;

    



    printf("where diamonds?");
    printf("so we back in the mine.");
    printf("got our pickaxes swingin from side to side.");
    printf("(side, side to side.)");
    printf("heads up!");
    printf("hear a sound, turn around and look up.");
    
    
    return 1; 
}

void yield() {
   
}

//3 points
void done() {
}

//3 points
tid_t join(tid_t thread) { 
    return 1; 

}
