test_that("'avnotebooks()' works", {
    namespace <- "foo"
    name <- "bar"

    expect_identical(
        .avnotebooks_runtime_path(name),
        path.expand(file.path("~", name, "edit"))
    )

    path <- with_mocked_bindings(
        .avnotebooks_workspace_path(namespace, name),
        avstorage = function(namespace, name)
            paste("gs:/", namespace, name, sep="/")
    )
    expect_identical(path, "gs://foo/bar/notebooks")
})
