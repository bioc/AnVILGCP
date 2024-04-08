test_that("'avnotebooks()' works", {
    namespace <- "foo"
    name <- "bar"

    expect_identical(
        .avnotebooks_runtime_path(name),
        path.expand(file.path("~", name, "edit"))
    )

    path <- with_mock(
        avstorage = function(namespace, name)
            paste("gs:/", namespace, name, sep="/"),
        .avnotebooks_workspace_path(namespace, name)
    )
    expect_identical(path, "gs://foo/bar/notebooks")
})
