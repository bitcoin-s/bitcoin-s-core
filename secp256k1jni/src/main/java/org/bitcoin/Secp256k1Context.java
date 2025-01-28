/*
 * Copyright 2014-2016 the libsecp256k1 contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.bitcoin;

import org.scijava.nativelib.NativeLoader;

/**
 * This class holds the context reference used in native methods 
 * to handle ECDSA operations.
 */
public class Secp256k1Context {
  private static final boolean enabled; //true if the library is loaded
  private static final long context; //ref to pointer to context obj

  static { //static initializer
      boolean isEnabled = true;
      long contextRef = -1;
      try {
          if (System.getProperty("os.name").startsWith("Windows")) {
              //our binary for windows has a different name than the linux/mac binary
              NativeLoader.loadLibrary("libsecp256k1-0");
          } else {
              NativeLoader.loadLibrary("secp256k1");
          }
          contextRef = secp256k1_init_context();
      } catch (java.io.IOException | UnsatisfiedLinkError e) {
          System.out.println("UnsatisfiedLinkError: " + e.toString());
          isEnabled = false;
      }
      enabled = isEnabled;
      context = contextRef;

      if (isEnabled) {
          Runtime.getRuntime().addShutdownHook(new Thread() {
              public void run() {
                  NativeSecp256k1.cleanup();
              }
          });
      }
  }

  /**
   * Detects whether or not the libsecp256k1 binaries were successfully
   * loaded in static initialization above. Useful in enabling a fallback
   * to Bouncy Castle implementations in the case of having no libsecp present.
   */
  public static boolean isEnabled() {
      return enabled;
  }

  public static long getContext() {
     if(!enabled) return -1; //sanity check
     return context;
  }

  private static native long secp256k1_init_context();
}
