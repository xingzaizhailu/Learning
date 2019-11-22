import sun.net.NetHooks;

/*
 * @lc app=leetcode id=82 lang=java
 *
 * [82] Remove Duplicates from Sorted List II
 */

// @lc code=start
/**
 * Definition for singly-linked list.
 * public class ListNode {
 *     int val;
 *     ListNode next;
 *     ListNode(int x) { val = x; }
 * }
 */
class Solution {
    public ListNode deleteDuplicates(ListNode head) {
        if (head == null) return null;

        do {
            head = stripLeftDupes(head);
            if (head == null) return null;
            if (head.next == null) return head;
        } while (head.val == head.next.val);

        ListNode newhead = head, tail = head, cur = head.next;

        while (cur != null) {
            cur = stripLeftDupes(cur);

            if (cur == null || cur.next == null) {
                tail.next = cur;
                break;
            } else if (cur.val != cur.next.val) {
                tail.next = cur;
                tail = cur;
                cur = cur.next;
            }
        }

        return newhead;
    }

    private ListNode stripLeftDupes(ListNode head) {
        if (head == null) return null;
        if (head.next == null) return head;

        ListNode cur = head.next;

        if (head.val != cur.val) return head;

        while (cur != null && cur.val == head.val) {
            cur = cur.next;
        }

        return cur;
    }
}
// @lc code=end

