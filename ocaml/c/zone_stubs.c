/* This stuff is courtesy the UPPAAL DBM library. */
#include <dbm/constraints.h>
#include <dbm/dbm.h>
#include <dbm/fed.h>
#include <dbm/print.h>
#include <base/bitstring.h>

#include <stdio.h>

#ifdef __cplusplus
using namespace dbm;

std::string getClockName(cindex_t index) 
{
  char str1[4];
  sprintf(str1, "x%d", index);
  return std::string(str1);
}

const char *dbm_toString(raw_t *dbm, cindex_t dim) 
{
  if (dbm_isEmpty(dbm, dim))
    {
      return "false";
    }

  uint32_t mingraph[bits2intsize(dim*dim)];
  size_t n = dbm_cleanBitMatrix(dbm, dim, mingraph,
                                dbm_analyzeForMinDBM(dbm, dim, mingraph));
  if (n == 0)
    {
      return "true";
    }

  char buffer[32];
  std::string str;
  bool firstConstraint = true;

  str += "(";
  for(cindex_t i = 0; i < dim; ++i)
    {
      for(cindex_t j = 0; j < dim; ++j)
        {
          if (base_readOneBit(mingraph, i*dim+j))
            {
              assert(i != j); // Not part of mingraph.

              // Conjunction of constraints.
              if (!firstConstraint)
                {
                  str += " && ";
                }
              firstConstraint = false;

              // Write constraints xi-xj <|<= DBM[i,j]
              if (i == 0)
                {
                  assert(j != 0); // 0,0 never part of mingraph
                  if (dbm_addFiniteRaw(dbm[j], dbm[j*dim]) == dbm_LE_ZERO)
                    {
                      assert(n);
                      n -= base_getOneBit(mingraph, j*dim);
                      base_resetOneBit(mingraph, j*dim);

                      // xj == b
                      snprintf(buffer, sizeof(buffer), "==%d",
                               -dbm_raw2bound(dbm[j]));
                      str += getClockName(j);
                      str += buffer;
                    }
                  else // b < xj, b <= xj
                    {
                      snprintf(buffer, sizeof(buffer), "%d%s",
                               -dbm_raw2bound(dbm[j]),
                               dbm_rawIsStrict(dbm[j]) ? "<" : "<=");
                      str += buffer;
                      str += getClockName(j);
                    }
                }
              else
                {
                  // xi-xj == b ?
                  if (dbm_addFiniteRaw(dbm[i*dim+j], dbm[j*dim+i]) == dbm_LE_ZERO)
                    {
                      assert(n);
                      n -= base_getOneBit(mingraph, j*dim+i);
                      base_resetOneBit(mingraph, j*dim+i);

                      // xi == xj
                      if (j > 0 && dbm[i*dim+j] == dbm_LE_ZERO)
                        {
                          str += getClockName(i);
                          str += "==";
                          str += getClockName(j);
                          goto consume_n;
                        }
                      // == b
                      snprintf(buffer, sizeof(buffer), "==%d",
                               dbm_raw2bound(dbm[i*dim+j]));
                    }
                  else
                    {
                      // xi < xj, xi <= xj
                      if (j > 0 && dbm_raw2bound(dbm[i*dim+j]) == 0)
                        {
                          str += getClockName(i);
                          str += dbm_rawIsStrict(dbm[i*dim+j]) ? "<" : "<=";
                          str += getClockName(j);
                          goto consume_n;
                        }
                      // < b, <= b
                      snprintf(buffer, sizeof(buffer), "%s%d",
                               dbm_rawIsStrict(dbm[i*dim+j]) ? "<" : "<=",
                               dbm_raw2bound(dbm[i*dim+j]));
                    }

                  if (j == 0) // xi < b, xi <= b
                    {
                      assert(i != 0); // 0,0 never part of mingraph
                      str += getClockName(i);
                      str += buffer;
                    }
                  else // xi-xj < b, xi-xj <= b
                    {
                      str += getClockName(i);
                      str += "-";
                      str += getClockName(j);
                      str += buffer;
                    }
                }
            consume_n:
              assert(n);
              --n; // Consumed one constraint.
              if (!n) goto stop_loops;
            }
        }
    }
 stop_loops:
  str += ")";

  return str.c_str();
}

extern "C" {
#endif

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

  /* Encapsulation of opaque handles (of type RAW_T *)
     as Caml custom blocks. */
  static struct custom_operations udbm_raw_t_ops = {
    "fr.inria.caml.udbm_raw_t",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
  };

  /* Encapsulation of opaque handles (of type CONSTRAINT_T *)
     as Caml custom blocks. */
  static struct custom_operations udbm_constraint_t_ops = {
    "fr.inria.caml.udbm_constraint_t",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
  };

  /* Accessing the RAW_T * part of a Caml custom block */
#define raw_t_val(v) (*((raw_t **) Data_custom_val(v)))

  /* Accessing the CONSTRAINT_T * part of a Caml custom block */
#define constraint_t_val(v) (*((constraint_t **) Data_custom_val(v)))

  /* Allocating a Caml custom block to hold the given RAW_T * */
  static value alloc_raw_t(raw_t * w)
  {
    value v = alloc_custom(&udbm_raw_t_ops, sizeof(raw_t *), 0, 1);
    raw_t_val(v) = w;
    return v;
  }

  /* Allocating a Caml custom block to hold the given CONSTRAINT_T * */
  static value alloc_constraint_t(constraint_t * w)
  {
    value v = alloc_custom(&udbm_constraint_t_ops, sizeof(constraint_t *), 0, 1);
    constraint_t_val(v) = w;
    return v;
  }

  CAMLprim value zone_dbm_init (value dim) {
    CAMLparam1(dim);
    raw_t *dbm = (raw_t *)malloc(Int_val(dim)*Int_val(dim)*sizeof(raw_t));
    dbm_init(dbm, Int_val(dim));
    CAMLreturn (alloc_raw_t(dbm));
  }

  /*No idea how this will ever be used.*/
  CAMLprim value zone_dbm_finish (value dbm) {
    CAMLparam1(dbm);
    free((raw_t *)raw_t_val(dbm));
    CAMLreturn (Val_unit);
  }

  CAMLprim value zone_dbm_constrainC (value dbm, value dim, value c) {
    CAMLparam3(dbm, dim, c);
    /* printf("(constraint_t_val(c))->i = %d\n", (constraint_t_val(c))->i); */
    /* printf("(constraint_t_val(c))->j = %d\n", (constraint_t_val(c))->j); */
    dbm_constrainC(raw_t_val(dbm), Int_val(dim), *(constraint_t_val(c)));
    CAMLreturn (dbm);
  }

  CAMLprim value zone_dbm_constraint2 (value i,
                                       value j,
                                       value bound,
                                       value isStrict) {
    CAMLparam4(i, j, bound, isStrict);
    constraint_t *constraint = (constraint_t *)malloc(sizeof(constraint_t *));
    *constraint = dbm_constraint2((cindex_t)Int_val(i),
                                  (cindex_t)Int_val(j),
                                  (int32_t)Int_val(bound),
                                  isStrict == Val_true ? TRUE : FALSE);
    CAMLreturn (alloc_constraint_t(constraint));
  }

  CAMLprim value zone_dbm_isEmpty (value dbm, value dim) {
    CAMLparam2(dbm, dim);
    if (dbm_isEmpty(raw_t_val(dbm), Int_val(dim)) == TRUE) {
      CAMLreturn (Val_int(1)); //ocaml true
    } else {
      CAMLreturn (Val_int(0)); //ocaml false
    }
  }

  CAMLprim value zone_dbm_haveIntersection (value dst, value src, value dim) {
    CAMLparam3(dst, src, dim);
    if (dbm_intersection(raw_t_val(dst), raw_t_val(src), Int_val(dim)) == TRUE) {
      CAMLreturn (Val_int(1)); //ocaml true
    } else {
      CAMLreturn (Val_int(0)); //ocaml false
    }
  }

  CAMLprim value zone_dbm_intersection (value dst, value src, value dim) {
    CAMLparam3(dst, src, dim);
    dbm_intersection(raw_t_val(dst), raw_t_val(src), Int_val(dim));
    CAMLreturn (dst);
  }

  CAMLprim value zone_dbm_freeClock (value dbm, value dim, value k) {
    CAMLparam3(dbm, dim, k);
    dbm_freeClock(raw_t_val(dbm), Int_val(dim), Int_val(k));
    CAMLreturn (dbm);
  }

  CAMLprim value zone_dbm_updateValue (value dbm, value dim, value k, value val) {
    CAMLparam4(dbm, dim, k, val);
    dbm_updateValue(raw_t_val(dbm), Int_val(dim), Int_val(k), Int_val(val));
    CAMLreturn (dbm);
  }

  CAMLprim value zone_dbm_up(value dbm, value dim) {
    CAMLparam2(dbm, dim);
    dbm_up(raw_t_val(dbm), Int_val(dim));
    CAMLreturn (dbm);
  }

  CAMLprim value zone_dbm_toString(value dbm, value dim) {
    CAMLparam2(dbm, dim);
    CAMLreturn (caml_copy_string(dbm_toString(raw_t_val(dbm), Int_val(dim))));
  }

  CAMLprim value zone_dbm_zero(value dbm, value dim) {
    CAMLparam2(dbm, dim);
    dbm_zero(raw_t_val(dbm), Int_val(dim));
    CAMLreturn (dbm);
  }

#ifdef __cplusplus
}
#endif
