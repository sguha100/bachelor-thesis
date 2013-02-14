#include <caml/mlvalues.h>
#include <caml/alloc.h>

/* This stuff is courtesy the UPPAAL DBM library. */
#include <dbm/constraints.h>
#include <dbm/dbm.h>

CAMLprim value init_zone (value numclocks) {
  raw_t dbm;
  cindex_t dim = Int_val (numclocks) + 1;
  dbm_init(&dbm, dim);
  char *s = (char *)malloc(sizeof(raw_t) + sizeof(char));
  unsigned int i;
  for (i = 0; i < (sizeof(raw_t) / sizeof(char)); i++) {
    s[i] = *((char *)&dbm + i);
  }
  s[i] = '\0';
  return caml_copy_string(s);
}

int main() {
  raw_t dbm;
  cindex_t dim = 2;
  dbm_init(&dbm, dim);
  return 0;
}
