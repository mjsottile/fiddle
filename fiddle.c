/*
 * template stuff that won't be autogenned.
 */

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include "fiddle.h"

FloatArray2D_t *allocateFloat2D(int nrows, int ncols) {
  float *tmp = malloc(sizeof(float)*nrows*ncols);
  float **a = malloc(sizeof(float *)*nrows);
  FloatArray2D_t *fa = malloc(sizeof(FloatArray2D_t));
  int i;

  for (i=0;i<nrows;i++) {
    a[i] = &tmp[i*ncols];
  }

  fa->data = a;
  fa->dimlower = malloc(sizeof(int)*2);
  fa->dimupper = malloc(sizeof(int)*2);
  fa->dimlower[0] = fa->dimlower[1] = 0;
  fa->dimupper[0] = nrows;
  fa->dimlower[1] = ncols;

  return fa;
}

void freeFloat2D(FloatArray2D_t *a) {
  float *ptr = a->data[0];
  free(ptr);
  free(a->data);
  free(a->dimlower);
  free(a->dimupper);
  free(a);
}
