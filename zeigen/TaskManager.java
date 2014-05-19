
import java.util.TreeMap;
import java.util.SortedMap;
import java.util.Map;
import java.util.Set;
import java.util.LinkedHashSet;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.Condition;


public class TaskManager implements Runnable{
    private final TaskPerformer performer;
    private TreeMap<Long, Set<Task>> tasks;
    private ReentrantLock lock;
    private Condition condition;

    public TaskManager(TaskPerformer performer) {
        this.performer = performer;
        this.lock = new ReentrantLock(true);
        this.lock.lock();
        this.condition = this.lock.newCondition();

        this.tasks = new TreeMap<Long, Set<Task>>();
        this.lock.unlock();
    }

    public void addTask(long time, String command) {
        this.addTask(time, command, "");
    }

    public void addTask(long time, String command, String options) {
        Task task = new Task(command, options);
        this.lock.lock();
        Set<Task> tasksAt = this.tasks.get(time);
        if (tasksAt == null) {
            tasksAt = new LinkedHashSet<Task>();
            this.tasks.put(time, tasksAt);
        }
        tasksAt.add(task);
        this.condition.signalAll();
        this.lock.unlock();
    }

    public int clear() {
        this.lock.lock();
        int size = this.size();
        this.tasks.clear();
        this.lock.unlock();
        return size;
    }

    public int size() {
        int size = 0;
        this.lock.lock();
        for (Set<Task> tasksAt : this.tasks.values()) {
            for (Task task : tasksAt) {
                size++;
            }
        }
        this.lock.unlock();
        return size;
    }

    public void process() {
        this.lock.lock();
        SortedMap<Long, Set<Task>> currentTasks = this.tasks.headMap(Time.now());
        for (Map.Entry<Long, Set<Task>> entry : currentTasks.entrySet()) {
            Set<Task> tasksAt = entry.getValue();
            this.tasks.remove(entry.getKey());
            for (Task task : tasksAt) {
                this.performer.doTask(task.command, task.options);
            }
        }
        this.lock.unlock();
    }

    public void run() {
        while (true) {
            process();

            this.lock.lock();

            try {
                if (this.tasks.size() == 0) {
                    this.condition.await();
                } else {

                    long epsilon = 1;
                    Long next = this.tasks.firstKey() - Time.now();
                    if (next > epsilon) {
                        this.condition.await(next, Time.timeUnit);
                    }
                }
            } catch (InterruptedException e) {
                System.out.println("TASKMANAGER WAS INTERRUPTED");
            }
            this.lock.unlock();
        }
    }

    private class Task {
        public final String command;
        public final String options;

        public Task(String command) {
            this(command, "");
        }
        public Task(String command, String options) {
            this.command = command;
            this.options = options;
        }
        public boolean equals(Object other) {
            if (other instanceof Task) {
                Task otherTask = (Task) other;
                return this.command.equals(otherTask.command) && this.options.equals(otherTask.options);
            }
            return false;
        }

        public int hashCode() {
            return (this.command + this.options).hashCode();
        }
    }
}
