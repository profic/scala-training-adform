package com.adform.task.scala_rb_tree;

import com.adform.task.Main;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;

/**
 * Created by vladislav.molchanov on 23.02.2016.
 */
public class ScalaRBTreeTestInJava {

    public static void main(String[] args) throws Exception {
        Options opt = new OptionsBuilder()
                .include(ScalaRBTreeTestInJava.class.getSimpleName())
                .forks(1)
                .build();
        new Runner(opt).run();
    }

    @Benchmark
    public static void doSearch() {
//        com.adform.task.scala_rb_tree_for_intervals_with_long_key.Main.doSearch();
        Main.doSearch();
    }

}
