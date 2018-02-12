#include <btBulletDynamicsCommon.h>
#include <btBulletCollisionCommon.h>

#include "cbullet.h"

extern "C" {

  broadphase_interface* new_broadphase_interface() {
    return reinterpret_cast<broadphase_interface*>(new btDbvtBroadphase());
  }
  
  void free_broadphase_interface(broadphase_interface* broadphase) {
    delete reinterpret_cast<btDbvtBroadphase*>(broadphase);
  }

  default_collision_configuration* new_default_collision_configuration() {
    return reinterpret_cast<default_collision_configuration*>(new btDefaultCollisionConfiguration());
  }

  void free_default_collision_configuration(default_collision_configuration* collision_configuration) {
    delete reinterpret_cast<btDefaultCollisionConfiguration*>(collision_configuration);
  }

  collision_dispatcher* new_collision_dispatcher(default_collision_configuration* collision_configuration) {
    return reinterpret_cast<collision_dispatcher*>
      (new btCollisionDispatcher
       (reinterpret_cast<btDefaultCollisionConfiguration*>
	(collision_configuration)));
  }

  void free_collision_dispatcher(collision_dispatcher* dispatcher) {
    delete reinterpret_cast<btCollisionDispatcher*>(dispatcher);
  }

  sequential_impulse_constraint_solver* new_sequential_impulse_constraint_solver() {
    return reinterpret_cast<sequential_impulse_constraint_solver*>(new btSequentialImpulseConstraintSolver());
  }
  
  void free_sequential_impulse_constraint_solver(sequential_impulse_constraint_solver* solver) {
    delete reinterpret_cast<btSequentialImpulseConstraintSolver*>(solver);
  }

  discrete_dynamics_world* new_discrete_dynamics_world
  (collision_dispatcher* dispatcher,
   broadphase_interface* broadphase,
   sequential_impulse_constraint_solver* solver,
   default_collision_configuration* collision_configuration) {
    return reinterpret_cast<discrete_dynamics_world*>(new btDiscreteDynamicsWorld
      (reinterpret_cast<btDispatcher*>(dispatcher),
       reinterpret_cast<btBroadphaseInterface*>(broadphase),
       reinterpret_cast<btConstraintSolver*>(solver),
       reinterpret_cast<btCollisionConfiguration*>(collision_configuration)));
  }

  void free_discrete_dynamics_world(discrete_dynamics_world* world) {
    delete reinterpret_cast<btDiscreteDynamicsWorld*>(world);
  }

  void set_gravity(discrete_dynamics_world* world, scalar x, scalar y, scalar z) {
    reinterpret_cast<btDiscreteDynamicsWorld*>(world)->setGravity(btVector3(x, y, z));
  }

  int step_simulation(discrete_dynamics_world* world, scalar time_step, const int* max_sub_steps, const scalar* fixed_time_step) {
    int mss = max_sub_steps ? *max_sub_steps : 1;
    scalar fts = fixed_time_step ? *fixed_time_step : (1.0f / 60.0f);
    return reinterpret_cast<btDiscreteDynamicsWorld*>(world)->stepSimulation(time_step, mss, fts);
  }
  
  void add_rigid_body(discrete_dynamics_world* world, rigid_body* rigid_body) {
    reinterpret_cast<btDiscreteDynamicsWorld*>(world)->addRigidBody(reinterpret_cast<btRigidBody*>(rigid_body));
  }
  
  int default_step_simulation(discrete_dynamics_world* world, scalar time_step) {
    return step_simulation(world, time_step, NULL, NULL);
  }

  static_plane_shape* new_static_plane_shape(scalar x,
					     scalar y,
					     scalar z,
					     scalar plane_constant) {
    return reinterpret_cast<static_plane_shape*>(new btStaticPlaneShape(btVector3(x, y ,z), plane_constant));
  }
  
  void free_static_plane_shape(static_plane_shape* static_plane_shape) {
    delete reinterpret_cast<btStaticPlaneShape*>(static_plane_shape);
  }
  
  sphere_shape* new_sphere_shape(scalar radius) {
    return reinterpret_cast<sphere_shape*>(new btSphereShape(radius));
  }

  void free_sphere_shape(sphere_shape* sphere_shape) {
    delete reinterpret_cast<btSphereShape*>(sphere_shape);
  }
  
  void calculate_local_inertia(collision_shape* collision_shape,
			       scalar mass,
			       scalar* x_out,
			       scalar* y_out,
			       scalar* z_out) {
    btVector3 inertia;
    reinterpret_cast<btCollisionShape*>(collision_shape)->calculateLocalInertia(mass, inertia);
    *x_out = inertia[0];
    *y_out = inertia[1];
    *z_out = inertia[2];
  }

}
