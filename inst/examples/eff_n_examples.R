gles17_weighted <- rakesvy(design = gles17, 
    gender ~ c("Male" = .495, "Female" = .505),
    eastwest ~ c("East Germany" = .195, "West Germany" = .805)
)

eff_n(gles17_weighted)
weight_eff(gles17_weighted)