

object RV32ETop extends App{
    (new chisel3.stage.ChiselStage).execute(args,
        Seq(chisel3.stage.ChiselGeneratorAnnotation(() => new top())
            // firrtl.stage.RunFirrtlTransformAnnotation(new AddModulePrefix()),
            // ModulePrefixAnnotation("ysyx_210539_")
        ))

    // (new chisel3.stage.ChiselStage).execute(args, Seq(chisel3.stage.ChiselGeneratorAnnotation(() => new newtop())))
    // (new chisel3.stage.ChiselStage).execute(args, Seq(chisel3.stage.ChiselGeneratorAnnotation(() => new VgaCrossbar())))
}