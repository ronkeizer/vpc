assert(
  "as_vpcdb requires type",
  tryCatch(as_vpcdb(a=1), error=function(e) e$message) ==
    "`type` must be specified"
)

assert(
  "as_vpcdb has a class for the type, vpcdb, and list",
  as_vpcdb(type="foo") %==%
    structure(
      list(
        type="foo",
        labeller=NULL,
        facet=NULL,
        scales=NULL
      ),
      class=c("vpcdb_foo", "vpcdb", "list")
    )
)

assert(
  "as_vpcdb requires valid facet, if given",
  tryCatch(as_vpcdb(type="foo", facet="foo"), error=function(e) e$message) ==
    "`facet` argument needs to be one of `wrap`, `columns`, or `rows`."
)
assert(
  "as_vpcdb changes facet 'grid' to 'rows'",
  as_vpcdb(type="foo", facet="grid") %==%
    structure(
      list(
        type="foo",
        labeller=NULL,
        facet="rows",
        scales=NULL
      ),
      class=c("vpcdb_foo", "vpcdb", "list")
    )
)

assert(
  "as_vpcdb requires valid scales, if given",
  tryCatch(as_vpcdb(type="foo", scales="foo"), error=function(e) e$message) ==
    "`scales` argument needs to be one of `fixed`, `free_y`, `free_x` or `free`."
)

