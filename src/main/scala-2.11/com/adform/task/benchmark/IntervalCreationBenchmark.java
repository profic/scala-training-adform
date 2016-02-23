package com.adform.task.benchmark;

import com.adform.tree.openjdk_interval_rb_tree.Interval;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

/**
 * Created by vladislav.molchanov on 23.02.2016.
 */
public class IntervalCreationBenchmark {

    public static void main(String[] args) throws RunnerException {
        Options opt = new OptionsBuilder()
                .include(IntervalCreationBenchmark.class.getSimpleName())
                .forks(1)
                .build();
        new Runner(opt).run();
    }

//    @Benchmark
    public static void intervalCreationBenchmark() {
        createInterval();
    }

    private static void createInterval() {
        Interval i = new Interval(1, 1);
    }

    @Benchmark
    public static void intervalCreationBenchmarkScala() {
        IntervalCreationBenchmarkScala.create();
    }

}
