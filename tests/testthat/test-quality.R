test_that("expected sample size and weight efficiency produce correct results", {
    # ---- svydesign object as input ----
    expect_equal(
        eff_n(survey::svydesign(~1, data = gles17, weights = benchmark_out)),
        (sum(benchmark_out) ^ 2) / sum(benchmark_out ^ 2)
    )
    
    expect_equal(
        weight_eff(survey::svydesign(~1, data = gles17, weights = benchmark_out)),
        ((sum(benchmark_out) ^ 2) / sum(benchmark_out ^ 2)) / sum(benchmark_out)
    )
    
    # ---- vector as input ----
    expect_equal(
        eff_n(benchmark_out),
        (sum(benchmark_out) ^ 2) / sum(benchmark_out ^ 2)
    )
    
    expect_equal(
        weight_eff(benchmark_out),
        ((sum(benchmark_out) ^ 2) / sum(benchmark_out ^ 2)) / sum(benchmark_out)
    )
})