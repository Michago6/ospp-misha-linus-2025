#include "bounded_buffer.h"

#include <string.h>         // strncmp()
#include <stdbool.h>        // true, false
#include <assert.h>         // assert()
#include <ctype.h>          // isprint()
#include <stddef.h>         // NULL
#include <stdio.h>          // printf(), fprintf()
#include <stdlib.h>         // [s]rand()
#include <unistd.h>         // usleep(), sleep()
#include <pthread.h>        // pthread_...

void buffer_init(buffer_t *buffer, int size) {

  // Allocate the buffer array.
  tuple_t *array = malloc(size*sizeof(tuple_t));

  if (array == NULL) {
    perror("Could not allocate buffer array");
    exit(EXIT_FAILURE);
  }

  // Initialize the binary mutex semaphore.
  buffer->mutex = psem_init(1);
  buffer->array = array;
  buffer->size = size;
  buffer->in = 0;
  buffer->out = 0;
  buffer->data = psem_init(0);
  buffer->empty = psem_init(size);

}

void buffer_destroy(buffer_t *buffer) {

  // Dealloacte the array.
  free(buffer->array);
  buffer->array = NULL;

  // Deallocate the mutex semaphore.
  psem_destroy(buffer->mutex);
  buffer->mutex = NULL;

  // Deallocate the data semaphore.
  psem_destroy(buffer->data);
  buffer->data = NULL;

  // Deallocate the empty semaphore.
  psem_destroy(buffer->empty);
  buffer->empty = NULL;
}

void buffer_print(buffer_t *buffer) {
  puts("");
  puts("---- Bounded Buffer ----");
  puts("");

  printf("size: %d\n", buffer -> size);
  printf("  in: %d\n", buffer -> in);
  printf(" out: %d\n", buffer -> out);
  puts("");


  // TODO: print all elements of the array.

  // Print element i of the array. 
  for (int i = 0; i < buffer->size; i++)
  {
    printf("array[%d]: (%d, %d)\n", i, buffer->array[i].a, buffer->array[i].b);
  }
  

  puts("");
  puts("------------------------");
  puts("");
}


void buffer_put(buffer_t *buffer, int a, int b) {
  //decrement empty
  psem_wait(buffer->empty);
  
  psem_wait(buffer->mutex);
  
  
  
  // Insert the tuple (a, b) into the buffer. 
  buffer->array[buffer->in].a = a;
  buffer->array[buffer->in].b = b;
  
  // TODO: Update buffer->in and make sure it wraps around.
  buffer->in = (buffer->in + 1) % buffer->size;
  
  //increment data
  psem_signal(buffer->data);
  
  psem_signal(buffer->mutex);
}

void buffer_get(buffer_t *buffer, tuple_t *tuple) {
  //decrement data
  psem_wait(buffer->data);

  psem_wait(buffer->mutex);


  
  // Read the tuple (a, b) from the buffer.
  tuple->a = buffer->array[buffer->out].a;
  tuple->b = buffer->array[buffer->out].b;
  
  
  buffer->out = (buffer->out + 1) % buffer->size; // TODO: Update buffer->out and make sure it wraps around.

  //increment empty
  psem_signal(buffer->empty);

  psem_signal(buffer->mutex);
}
