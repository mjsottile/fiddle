#ifndef __FIDDLE_H__
#define __FIDDLE_H__

typedef struct FloatArrayHandle2D {
  float **data;
  int *dimlower;
  int *dimupper;
} FloatArray2D_t;

typedef struct IntArrayHandle2D {
  int **data;
  int *dimlower;
  int *dimupper;
} IntArray2D_t;

FloatArray2D_t *allocateFloat2D(int nrows, int ncols);
void freeFloat2D(FloatArray2D_t *a);
IntArray2D_t *allocateInt2D(int nrows, int ncols);
void freeInt2D(IntArray2D_t *a);

#endif /* __FIDDLE_H__ */
