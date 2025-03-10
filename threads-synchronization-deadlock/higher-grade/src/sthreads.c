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
// thread_t *latest_thread = NULL;
tid_t global_tid = 0;
thread_t *running_thread = NULL;
bool thread_executing = false;

typedef struct ready_queue {
    size_t size;
    thread_t *first_thread;
    thread_t *last_thread;
}ready_queue_t;

ready_queue_t *ready_queue;

// typedef struct waiting_queue {
//     thread_t *first_thread;
//     thread_t *latest_thread;
//     // thread_t *last_thread;
// }waiting_queue_t;











/*******************************************************************************
                             Auxiliary functions

                      Add internal helper functions here.
********************************************************************************/
/* FCFS */
void enqueue(thread_t *thread) {
    ready_queue->size++;

    if(ready_queue->size == 1) {
        ready_queue->first_thread = thread;
        ready_queue->last_thread = thread;
    } else {
        ready_queue->last_thread->next = thread;
        ready_queue->last_thread = thread;
    }
    thread->next = NULL;
}
/* FCFS */
thread_t *dequeue() {
    thread_t *to_return;
    if(ready_queue->size > 1) {
        ready_queue->size--;
        to_return = ready_queue->first_thread;
        
        ready_queue->first_thread = to_return->next;
    
        return to_return;
    } else if (ready_queue->size == 1) {
        ready_queue->size--;
        to_return = ready_queue->first_thread;

        ready_queue->first_thread = NULL;
        ready_queue->last_thread = NULL;
        return to_return;
        //if size < 1
    } else {
        return NULL;
    }
}

/*******************************************************************************
                    Implementation of the Simple Threads API
********************************************************************************/
int init() { 

    ready_queue = malloc(sizeof(ready_queue_t));
    if (!ready_queue) return -1;

    return 1;
}

tid_t spawn(void (*start)()) { 

    thread_t *thread = malloc(sizeof(thread_t));
    if (thread == NULL) return -1;
    thread->tid = global_tid;
    global_tid++;
    thread->state = ready;
    // thread->next = ready_queue->last_thread;
    
    thread->ctx.uc_stack.ss_sp = malloc(STACK_SIZE);  // Allocate stack space and update stack pointer
    thread->ctx.uc_stack.ss_size = STACK_SIZE; // Notify context of stack size allocated
    
    if(getcontext(&(thread->ctx)) == -1 ) {
        return -1;
    }
    
    
    makecontext(&(thread->ctx), start, 0);
    
    
    // if (ready_queue->first_thread == NULL) { ready_queue->first_thread = thread; };
    // ready_queue->last_thread = thread;
    enqueue(thread);
    //printf("readyqueue size: %ld\n", ready_queue->size);

    // printf("where diamonds?");
    // printf("so we back in the mine.");
    // printf("got our pickaxes swingin from side to side.");
    // printf("(side, side to side.)");
    // printf("heads up!");
    // printf("hear a sound, turn around and look up.");
    
    
    return thread->tid; 
}

void yield() {


    // //if running_thread is null then execute it
    // if(!running_thread) {
    //     //printf("no running thread...\n");
    //     thread_t *to_run = dequeue();
    //     if(!to_run) {
    //         //printf("no process to run");
    //         return;
    //     }
    //     to_run->state = running;
    //     running_thread = to_run;
    //     setcontext(&(to_run->ctx));
    // }



    //printf("yiled");
    // if(ready_queue->size < 1) {
    //     return;
    // }
   // printf("readyqueue size: %ld\n", ready_queue->size);
    thread_t *to_run = dequeue();
    if(!to_run) {
        printf("ERROR: no process to run!, readyqueue size: %ld\n", ready_queue->size);
        
        return;
    }
    to_run->state = running;
    //printf("två första klara");

    if (thread_executing) {
        // printf("nu ska vi öka size\n");
        enqueue(running_thread);
        // printf("enqueu\n");

        thread_t *old_thread = running_thread;
        
        running_thread = to_run;

        old_thread->state = ready;
        swapcontext(&(old_thread->ctx), &(to_run->ctx));
    } else {
        thread_executing = true;
        // printf("set context\n");


        running_thread = to_run;

        setcontext(&(to_run->ctx));
    }
    //printf("running thread initializer\n");


}

//3 points
void done() {
}

//3 points
tid_t join(tid_t thread) { 
    return 1; 

}
