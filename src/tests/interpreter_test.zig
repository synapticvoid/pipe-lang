  const std = @import("std");
  const helpers = @import("helpers");

  test "evaluate factor" {
      var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
      defer arena.deinit();
      const result = try helpers.evaluate("3 + 2 * 4;", arena.allocator());
      try std.testing.expectEqual(11.0, result.number);
  }

  test "evaluate comparison" {
      var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
      defer arena.deinit();
      const result = try helpers.evaluate("3 + 2 > 4;", arena.allocator());
      try std.testing.expectEqual(true, result.boolean);
  }

  test "evaluate equality" {
      var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
      defer arena.deinit();
      const result = try helpers.evaluate("3 + 2 == 4;", arena.allocator());
      try std.testing.expectEqual(false, result.boolean);
  }