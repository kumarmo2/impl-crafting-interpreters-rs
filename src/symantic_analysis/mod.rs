pub(super) mod resolver;

//TODO:
// 1. feat: probably we can check the  that the return statements are inside functions during
//    symantic analysis.
// 2. can the below code can be resolved during symantic analysis.
//   ```
//        var a = "sd";
//        var a = "b"; // right now this is given as error only during execution phase.
//
//   ```
