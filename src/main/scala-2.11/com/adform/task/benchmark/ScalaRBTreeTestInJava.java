package com.adform.task.benchmark;

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

//    @Benchmark
//    public void createTreeFunctionalScala() {
//        BenchmarkTreeCreationHelper.createTreeFunctional();
//    }
//
//    @Benchmark
//    public void createTreeImperativeScala() {
//        BenchmarkTreeCreationHelper.createTreeImperative();
//    }
//
//
//    @Benchmark
//    public static void searchArrayBuffer() {
//        Main.searchArrayBuffer();
//    }
//    @Benchmark
//    public static void searchArrayBufferWithoutToList() {
//        Main.searchArrayBufferWithoutToList();
//    }
//
//    @Benchmark
//    public static void searchList() {
//        Main.searchList();
//    }

    @Benchmark
    public static void searchListTailRecursive() {
        Main.searchListTailRecursive();
    }

    @Benchmark
    public static void searchListTailRecursiveWithoutUglyIfElse() {
        Main.searchListTailRecursiveWithoutUglyIfElse();
    }
//
//    @Benchmark
//    public void searchListTailRecursiveParallel() {
//        Main.searchListTailRecursiveParallel();
//    }
}
