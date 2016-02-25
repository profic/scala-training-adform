package com.adform.task.benchmark;

import com.adform.task.Main;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

/**
 * Created by vladislav.molchanov on 23.02.2016.
 */
public class ScalaTestMutableMapVsImmutable {

    public static void main(String[] args) throws RunnerException {
        Options opt = new OptionsBuilder()
                .include(ScalaTestMutableMapVsImmutable.class.getSimpleName())
                .forks(1)
                .build();
        new Runner(opt).run();
    }

    @Benchmark
    public static void readMutableMap() {
        Main.readMutableMap();
    }

    @Benchmark
    public static void readImmutableMap() {
        Main.readImmutableMap();
    }

}
