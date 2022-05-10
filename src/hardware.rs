use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::sync::Arc;

pub trait ClockHandler {
    fn cycle(&self);
}

impl<T: Fn()> ClockHandler for T {
    fn cycle(&self) {
        (self)()
    }
}

pub struct Wire<T>(Arc<Cell<T>>);

impl<T> Wire<T> {
    pub fn new(init: T) -> Self {
        Wire(Arc::new(Cell::new(init)))
    }
}

impl<T> Clone for Wire<T> {
    fn clone(&self) -> Self {
        Wire(self.0.clone())
    }
}

impl<T> PartialEq for Wire<T> {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl<T: Copy> Wire<T> {
    pub fn value(&self) -> T {
        self.0.get()
    }

    pub fn set(&self, value: T) {
        self.0.set(value)
    }
}

#[derive(Debug)]
pub enum SystemBuildError {
    DeviceLoop(Box<str>),
}

pub struct SystemBuilder<T> {
    devices: Vec<Device<T>>,
    clock_handlers: Vec<Arc<dyn ClockHandler>>,
}

impl<T> SystemBuilder<T> {
    pub fn new() -> Self {
        SystemBuilder {
            devices: vec![],
            clock_handlers: vec![],
        }
    }

    pub fn add_device(
        &mut self,
        name: impl Into<Box<str>>,
        inputs: &[&Wire<T>],
        outputs: &[&Wire<T>],
        f: impl 'static + Fn(&[T], &mut Vec<T>),
    ) {
        self.devices.push(Device {
            inputs: inputs.iter().map(|&w| w.clone()).collect(),
            outputs: outputs.iter().map(|&w| w.clone()).collect(),
            function: Arc::new(f),
            name: name.into(),
        })
    }

    pub fn make_clock_handler(&mut self, f: impl 'static + ClockHandler) {
        let f: Arc<dyn ClockHandler> = Arc::new(f);
        self.add_clock_handler(f)
    }

    pub fn add_clock_handler(&mut self, f: impl Into<Arc<dyn ClockHandler>>) {
        self.clock_handlers.push(f.into())
    }

    pub fn build(mut self) -> Result<System<T>, SystemBuildError> {
        self.order_devices()?;
        Ok(System {
            devices: self.devices,
            clock_handlers: self.clock_handlers,
        })
    }

    fn order_devices(&mut self) -> Result<(), SystemBuildError> {
        let device_graph = DeviceGraph::new(&self.devices);
        device_graph.has_cycle().map_err(|devid| {
            SystemBuildError::DeviceLoop(
                self.devices
                    .iter()
                    .find(|d| d.id() == devid)
                    .map(|d| d.name.clone())
                    .unwrap(),
            )
        })?;
        self.devices
            .sort_unstable_by_key(|dev| device_graph.dependency_depth(dev.id()));
        Ok(())
    }
}

struct DeviceId<T>(*const dyn Fn(&[T], &mut Vec<T>));

impl<T> Debug for DeviceId<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "D({:p})", self.0)
    }
}
impl<T> Copy for DeviceId<T> {}
impl<T> Clone for DeviceId<T> {
    fn clone(&self) -> Self {
        DeviceId(self.0)
    }
}
impl<T> Eq for DeviceId<T> {}
impl<T> PartialEq for DeviceId<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<T> Hash for DeviceId<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

pub struct Device<T> {
    name: Box<str>,
    inputs: Vec<Wire<T>>,
    outputs: Vec<Wire<T>>,
    function: Arc<dyn Fn(&[T], &mut Vec<T>)>,
}

impl<T> Device<T> {
    fn depends_on(&self, other: &Self) -> bool {
        for inp in &self.inputs {
            if other.outputs.contains(inp) {
                return true;
            }
        }
        false
    }

    fn id(&self) -> DeviceId<T> {
        DeviceId(Arc::as_ptr(&self.function))
    }
}

pub struct DeviceGraph<T> {
    dependencies: HashMap<DeviceId<T>, HashSet<DeviceId<T>>>,
}

impl<T> DeviceGraph<T> {
    fn new(devices: &[Device<T>]) -> Self {
        let mut dependencies = HashMap::new();

        for dev1 in devices {
            let mut deps = HashSet::new();
            for dev2 in devices {
                if dev1.depends_on(dev2) {
                    deps.insert(dev2.id());
                }
            }
            dependencies.insert(dev1.id(), deps);
        }

        DeviceGraph { dependencies }
    }

    fn dependency_depth(&self, dev: DeviceId<T>) -> usize {
        self.dependencies[&dev]
            .iter()
            .map(|&dep| 1 + self.dependency_depth(dep))
            .max()
            .unwrap_or(0)
    }

    fn has_cycle(&self) -> Result<(), DeviceId<T>> {
        let mut visited = HashSet::new();
        let mut finished = HashSet::new();

        for start in self.dependencies.keys() {
            if visited.contains(start) {
                continue;
            }

            self.find_cycle(*start, &mut visited, &mut finished)?;
        }

        Ok(())
    }

    fn find_cycle(
        &self,
        dev: DeviceId<T>,
        visited: &mut HashSet<DeviceId<T>>,
        finished: &mut HashSet<DeviceId<T>>,
    ) -> Result<(), DeviceId<T>> {
        if finished.contains(&dev) {
            return Ok(());
        }

        if !visited.insert(dev) {
            return Err(dev);
        }

        for dep in &self.dependencies[&dev] {
            self.find_cycle(*dep, visited, finished)?;
        }

        finished.insert(dev);
        Ok(())
    }
}

pub struct System<T> {
    devices: Vec<Device<T>>,
    clock_handlers: Vec<Arc<dyn ClockHandler>>,
}

impl<T> Debug for System<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<System with {} devices", self.devices.len())
    }
}

impl<T: Copy> System<T> {
    pub fn simulate(&self) {
        for dev in &self.devices {
            let inp: Vec<T> = dev.inputs.iter().map(Wire::value).collect();
            let mut out = vec![];
            (dev.function)(&inp, &mut out);
            for (wire, value) in dev.outputs.iter().zip(out) {
                wire.set(value)
            }
        }
    }
}

impl<T> ClockHandler for System<T> {
    fn cycle(&self) {
        for handler in &self.clock_handlers {
            handler.cycle();
        }
    }
}

pub struct MemoryCell<T> {
    input_buffer: Cell<T>,
    output_buffer: Cell<T>,
}

impl<T: Copy> MemoryCell<T> {
    pub fn new(init: T) -> Self {
        MemoryCell {
            input_buffer: Cell::new(init),
            output_buffer: Cell::new(init),
        }
    }

    pub fn load(&self, input: T) {
        self.input_buffer.set(input)
    }

    pub fn fetch(&self) -> T {
        self.output_buffer.get()
    }
}

impl<T: Copy> ClockHandler for MemoryCell<T> {
    fn cycle(&self) {
        self.output_buffer.set(self.input_buffer.get());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_constant() {
        let mut sb = SystemBuilder::new();
        let x = Wire::new(0);
        sb.add_device("const", &[], &[&x], |_, out| out.push(42));
        let system = sb.build().unwrap();
        system.simulate();
        assert_eq!(x.value(), 42);
    }

    #[test]
    fn build_single_input_output() {
        let mut sb = SystemBuilder::new();
        let x = Wire::new(0);
        let y = Wire::new(0);
        sb.add_device("const", &[], &[&x], |_, out| out.push(1));
        sb.add_device("inv", &[&x], &[&y], |inp, out| out.push(-inp[0]));
        let system = sb.build().unwrap();
        system.simulate();
        assert_eq!(y.value(), -1);
    }

    #[test]
    fn devices_simulate_in_correct_order() {
        let mut sb = SystemBuilder::new();
        let x = Wire::new(0);
        let y = Wire::new(0);
        let z = Wire::new(0);
        sb.add_device("mul", &[&x, &y], &[&z], |inp, out| {
            out.push(inp[0] * inp[1])
        });
        sb.add_device("inv", &[&x], &[&y], |inp, out| out.push(-inp[0]));
        sb.add_device("const", &[], &[&x], |_, out| out.push(2));
        let system = sb.build().unwrap();
        system.simulate();
        assert_eq!(z.value(), -4);
    }

    #[test]
    fn device_loops_are_forbidden() {
        let mut sb = SystemBuilder::new();
        let x = Wire::new(0);
        let y = Wire::new(0);
        let z = Wire::new(0);
        sb.add_device("nop1", &[&x], &[&y], |_, _| {});
        sb.add_device("nop2", &[&y], &[&x], |_, _| {});
        sb.add_device("nop3", &[&y], &[&z], |_, _| {});
        match sb.build() {
            Err(SystemBuildError::DeviceLoop(dev)) => {
                assert!(&*dev == "nop1" || &*dev == "nop2");
            }
            _ => panic!("Expected cycle error"),
        }
    }

    #[test]
    fn clock_handlers() {
        let mut sb = SystemBuilder::<i32>::new();
        let cycles = Arc::new(Cell::new(0));
        {
            let cycles = cycles.clone();
            sb.make_clock_handler(move || cycles.set(cycles.get() + 1));
        }
        let system = sb.build().unwrap();
        assert_eq!(cycles.get(), 0);
        system.cycle();
        assert_eq!(cycles.get(), 1);
        system.cycle();
        assert_eq!(cycles.get(), 2);
    }

    #[test]
    fn delayed_loops_are_ok() {
        let mut sb = SystemBuilder::<i32>::new();

        let x = Wire::new(0);
        let y = Wire::new(0);
        let z = Wire::new(0);

        let mc = Arc::new(MemoryCell::new(1));
        let mc1 = mc.clone();
        let mc2 = mc.clone();
        let mch: Arc<dyn ClockHandler> = mc;

        sb.add_device("load", &[&x], &[], move |inp, _| mc1.load(inp[0]));
        sb.add_device("fetch", &[], &[&y], move |_, out| out.push(mc2.fetch()));
        sb.add_clock_handler(mch);
        sb.add_device("loop", &[&y], &[&x], |inp, out| out.push(-inp[0]));
        sb.add_device("mult", &[&y], &[&z], |inp, out| out.push(2 * inp[0]));

        let system = sb.build().unwrap();
        system.simulate();
        assert_eq!(z.value(), 2);
        system.cycle();
        system.simulate();
        assert_eq!(z.value(), -2);
        system.cycle();
        system.simulate();
        assert_eq!(z.value(), 2);
    }
}
