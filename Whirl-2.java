//package net.coljac.whirl;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;

/**
 * A Java implementaion of the Whirl programming language designed by BigZaphod
 * See http://www.bigzaphod.org/whirl/
 *
 * This implementation by coljac (http://coljac.net)
 * Date: Jul 10, 2004
 * This code is in the public domain.
 *
 * To compile: javac Whirl.java
 * To use: java -cp . Whirl [input file]
 */

public class Whirl {

  private int c; // For IO

  private int[] memory = new int[1000];


  private static final int OPS = 0;
  private static final int MATH = 1;

  private int ring = OPS;
  private boolean[] clockWise = new boolean[]{true, true};
  private int[] index = new int[]{0, 0};
  private int[] value = new int[]{0, 0};

  private int progIndex = 0;
  private int memIndex = 0;
  private String program = "";
  private int lastInstruction = -1;

  private InputStream stdin;

  private String[][] opsNames = new String[][]{
    {"Noop", "Exit", "One", "Zero", "Load", "Store", "PAdd", "DAdd", "Logic", "If", "IntIO", "AscIO"},
    {"Noop", "Load", "Store", "Add", "Mult", "Div", "Zero", "<", ">", "=", "Not", "Neg"}};

  public Whirl(String fileName) {
    try {
      BufferedReader br = new BufferedReader(new FileReader(fileName));
      String line = "";
      StringBuffer prog = new StringBuffer();
      while ((line = br.readLine()) != null) {
        if (!line.startsWith("//")) {
          line = line.replaceAll("\\s", "");
          line = line.replaceAll("//.*", "");
          prog.append(line.trim());
        }
      }
      br.close();
      program = prog.toString();
    } catch (IOException e) {
      System.err.println("Can't read source file: " + e.getMessage());
      System.exit(1);
    }
    stdin = System.in;
  }

  public void run() {
    while (progIndex < program.length()) {
      execute(Integer.parseInt("" + program.charAt(progIndex)));
    }
  }

  private void execute(int instruction) {
    progIndex++;
    switch (instruction) {
      case 1:
        index[ring] += (clockWise[ring] ? 1 : -1);
        if (index[ring] < 0) {
          index[ring] = opsNames[ring].length - 1;
        } else if (index[ring] > opsNames[ring].length - 1) {
          index[ring] = 0;
        }
        lastInstruction = 1;
        break;
      case 0:
        // Reverse
        clockWise[ring] = !clockWise[ring];
        if (lastInstruction == 0) {
          if (ring == OPS) {
            ops(index[OPS]);
          } else {
            math(index[MATH]);
          }
          // Swap rings
          if (ring == OPS) {
            ring = MATH;
          } else {
            ring = OPS;
          }
          lastInstruction = -1;
        } else {
          lastInstruction = 0;
        }
        break;
    }
  }

  private void ops(int operation) {
    switch (operation) {
      case 0: // noop
        break;
      case 1: // Exit
        System.exit(0);
        break;
      case 2: // One
        value[OPS] = 1;
        break;
      case 3: // Zero
        value[OPS] = 0;
        break;
      case 4: // Load
        value[OPS] = memory[memIndex];
        break;
      case 5: // Store
        memory[memIndex] = value[OPS];
        break;
      case 6: // PAdd
        progIndex += value[OPS];
        progIndex--;
        break;
      case 7: // DAdd
        memIndex += value[OPS];
        while (memIndex > memory.length - 1) {
          addMemory();
        }
        break;
      case 8: // Logic
        if (memory[memIndex] == 0) {
          value[OPS] = 0;
        } else {
          value[OPS] = (value[OPS] == 0 ? 0 : 1);
        }
        break;
      case 9: // if
        if (memory[memIndex] != 0) {
          progIndex += value[OPS];
          progIndex--;
        }
        break;
      case 10: // IntIO
        if (value[OPS] != 0) {
          System.out.print("" + memory[memIndex]);
        } else {
          try {
            memory[memIndex] = readInt();
          } catch (Exception e) {
            System.err.println("Error reading input:\n");
            e.printStackTrace();
          }
        }
        break;
      case 11: // AscIO
        if (value[OPS] != 0) {
          System.out.print("" + ((char) memory[memIndex]));
        } else {
          try {
            memory[memIndex] = readChar();
          } catch (Exception e) {
            System.err.println("Error reading input:\n");
            e.printStackTrace();
          }
        }
        break;
    }
  }

  private void math(int operation) {
    switch (operation) {
      case 0: // noop
        break;
      case 1: // Load
        value[MATH] = memory[memIndex];
        break;
      case 2: // Store
        memory[memIndex] = value[MATH];
        break;
      case 3: // Add
        value[MATH] += memory[memIndex];
        break;
      case 4: // Mult
        value[MATH] *= memory[memIndex];
        break;
      case 5: // Div
        value[MATH] /= memory[memIndex];
        break;
      case 6: // Zero
        value[MATH] = 0;
        break;
      case 7: // <
        if(value[MATH] < memory[memIndex]) {
          value[MATH] = 1;
        } else {
          value[MATH] = 0;
        }
        break;
      case 8: // >
        if(value[MATH] > memory[memIndex]) {
          value[MATH] = 1;
        } else {
          value[MATH] = 0;
        }
        break;
      case 9: // =
        if(value[MATH] == memory[memIndex]) {
          value[MATH] = 1;
        } else {
          value[MATH] = 0;
        }
        break;
      case 10: // Not
        if(value[MATH]!=0) {
          value[MATH] = 0;
        } else {
          value[MATH] = 1;
        }
        break;
      case 11: // Neg
        value[MATH] *= -1;
        break;
    }
  }

  private void addMemory() {
    int[] newMemory = new int[memory.length + 1000];
    System.arraycopy(memory, 0, newMemory, 0, memory.length);
    memory = newMemory;
  }

  // IO Stuff
  private char readChar() {
    try {
      c = System.in.read();
      return (char) c;
    } catch (IOException e) {
      c = -1;
    }
    return (char) 0x0;
  }

  private int readInt() {
    StringBuffer s = new StringBuffer();

    // eat up whitespace
    while (!(c == -1) && Character.isWhitespace((char) c))
      readChar();

    // now get the string
    while (!(c == -1) && !Character.isWhitespace((char) c)) {
      s.append((char) c);
      readChar();
    }

    if (s.length() == 0)
      throw new RuntimeException("Tried to read from empty stdin");
    else
      return Integer.parseInt(s.toString().trim());
  }

  public static void main(String[] args) {
    if(args.length!=1) {
      System.out.println("Usage: Whirl <input.wrl>\n");
      System.exit(0);
    }
    try {
      new Whirl(args[0]).run();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

}
